(ns muance.core
  (:require [cljs.analyzer :as ana]
            [clojure.string :as string])
  (:import [cljs.tagged_literals JSValue]))

(defonce typeid (atom 0))

(defn inc-typeid [t]
  ;; MAX_SAFE_INTEGER
  (if (= t 9007199254740991) 0 (inc t)))

(defn safe-symbol [x]
  (when x (symbol x)))

(defn resolve-namespace
  [env sym ns]
  (if (= 'cljs.core sym)
    sym
    (-> (merge (:requires ns) (:require-macros ns))
        (get sym))))

(defn cljs-resolve [env sym]
  (when (and sym (not (get-in env [:locals sym])))
    (let [sym-ns (safe-symbol (namespace sym))
          ns-name (if sym-ns
                    (->> (:ns env) (resolve-namespace env sym-ns))
                    (get-in env [:ns :name]))]
      (symbol (str ns-name) (name sym)))))

(declare compile-element-macro)
(declare text)

(defn compile-form [env form]
  (cond (and (seq? form) (symbol? (first form)))
        (let [var (cljs-resolve env (first form))
              clj-var (resolve var)]
          (cond (::tag (meta clj-var))
                (compile-element-macro env (::tag (meta clj-var)) nil (rest form))
                (= #'text clj-var) `(muance.core/text-node ~@(rest form))
                :else form))
        (string? form) `(muance.core/text-node ~form)
        :else form))

(defn local-dep [{name :name fn-var :fn-var
                  local :local init :init
                  op :op tag :tag dynamic :dynamic
                  ns :ns :as info}]
  (cond
    ;; fn param
    (and (= :var op) (not local) (not fn-var))
    info
    ;; local state
    (and (= name 'muance.core/*state*) (= dynamic true) (= ns 'muance.core))
    info
    ;; The analyzed var is a local var with an initial binding. The
    ;; initial binding may itself depend on a function parameter,
    ;; thus we recurse on the initial binding
    (and (= :var op) local init)
    (if (not (empty? (:children init)))
      ;; If init has children, then it is a complex expression, by
      ;; opposition to a single var or local binding. Recurse on every
      ;; child in order to analyze each expression individually
      (->> (:children init) (mapv :info) (some local-dep))
      (recur (:info init)))
    :else nil))

(defn static-symbol? [env s]
  (if-let [local (get (:locals env) s)]
    (not (local-dep local))
    (not (= 'muance.core/*state* (cljs-resolve env s)))))

(defn static? [env x]
  (cond (nil? x) true
        (boolean? x) true
        (string? x) true
        (keyword? x) true
        (number? x) true
        (and (seq? x) (= (first x) `quote)) true
        (vector? x) (every? (partial static? env) x)
        (map? x) (every? (partial static? env) x)
        (symbol? x) (static-symbol? env x)
        :else false))

(defn as-str [x]
  (cond (string? x) x
        (keyword? x) (name x)
        :else `(cljs.core/str ~x)))

(def attr-as-prop
  {:class :className
   :for :htmlFor
   :checked :checked
   :multiple :multiple
   :muted :muted
   :selected :selected
   :value :value})

(defn attributes [body]
  (let [attrs (->> (partition 2 body)
                   (take-while (comp keyword? first)))]
    (when (not (empty? attrs))
      (assert (apply distinct? (map first attrs))
              (str "duplicate attributes: " (pr-str (map first attrs)))))
    (into {} (map vec attrs))))

(defn handler? [h]
  (and (vector? h) (keyword? (first h))))

(defn validate-attributes [{:keys [hooks styles on] :as attributes}]
  (when (contains? attributes :hooks) (assert (map? hooks)))
  (when (contains? attributes :styles) (assert (map? styles)))
  (when (contains? attributes :on) (assert (or (handler? on) (every? handler? on)))))

(defn body-without-attributes [body attributes]
  (drop (* 2 (count attributes)) body))

(defn class-call [env class]
  (if (vector? class)
    (if (every? (partial static? env) class)
      `(prop-static "className" ~(reduce #(if (nil? %1) (str %2) (str %1 " " %2)) nil class))
      (let [classes-with-spaces (-> class (interleave (repeat " ")) butlast)]
        `(prop "className" (cljs.core/str ~@classes-with-spaces))))
    (if (static? env class)
      `(prop-static "className" ~class)
      `(prop "className" ~class))))

(defn style-calls [env styles]
  (map (fn [[k v]]
         (if (string/starts-with? (str k) ":--")
           (if (static? env v)
             `(style-custom-static ~(as-str k) ~v)
             `(style-custom ~(as-str k) ~v))
           (if (static? env v)
             `(style-static ~(as-str k) ~v)
             `(style ~(as-str k) ~v))))
       styles))

(defn on-calls [env ons]
  (let [static? (partial static? env)
        ons (if (handler? ons) [ons] ons)]
    (map (fn [[k f & args]]
           (if (and (static? f) (every? static? args))
             (let [l (count args)]
               (cond (= 0 l) `(on-static ~(as-str k) ~f)
                     (= 1 l) `(on-static1 ~(as-str k) ~f ~(nth args 0))
                     (= 2 l) `(on-static2 ~(as-str k) ~f ~(nth args 0) ~(nth args 1))
                     :else `(on-static3 ~(as-str k) ~f
                                        ~(nth args 0)
                                        ~(nth args 1)
                                        ~(nth args 2))))
             (let [l (count args)]
               (cond (= 0 l) `(on ~(as-str k) ~f)
                     (= 1 l) `(on1 ~(as-str k) ~f ~(nth args 0))
                     (= 2 l) `(on2 ~(as-str k) ~f ~(nth args 0) ~(nth args 1))
                     :else `(on3 ~(as-str k) ~f
                                 ~(nth args 0)
                                 ~(nth args 1)
                                 ~(nth args 2))))))
         ons)))

(defn attribute-calls [env tag attrs]
  (reduce (fn [calls [k v]]
            (cond
              (= k :key) calls
              (= k :hooks) calls
              (= k :class) (conj calls (class-call env v))
              (= k :styles) (into calls (style-calls env v))
              (= k :on)  (into calls (on-calls env v))
              (and (= tag "input") (= k :value))
              (conj calls (if (static? env v)
                            `(prop-static "value" ~v)
                            `(input-value ~v)))
              (contains? attr-as-prop k)
              (conj calls (if (static? env v)
                            `(prop-static ~(as-str (get attr-as-prop k)) ~v)
                            `(prop ~(as-str (get attr-as-prop k)) ~v)))
              (string/starts-with? (str k) ":xlink")
              (conj calls (if (static? env v)
                            `(attr-ns-static xlink-ns ~(as-str k) ~v)
                            `(attr-ns xlink-ns ~(as-str k) ~v)))
              (string/starts-with? (str k) ":xml")
              (conj calls (if (static? env v)
                            `(attr-ns-static xml-ns ~(as-str k) ~v)
                            `(attr-ns xml-ns ~(as-str k) ~v)))
              :else (conj calls (if (static? env v)
                                  `(attr-static ~(as-str k) ~v)
                                  `(attr ~(as-str k) ~v)))))
          '() attrs))

(defn with-svg-namespace [tag body]
  (case tag
    "svg" `(do
             (set! *svg-namespace* (cljs.core/inc *svg-namespace*))
             ~@body
             (set! *svg-namespace* (cljs.core/dec *svg-namespace*)))
    ;; *svg-namespace* is set to 0 in open-impl
    "foreignObject" `(let [parent-svg-namespace# *svg-namespace*]
                       ~@body
                       (set! *svg-namespace* parent-svg-namespace#))
    `(do ~@body)))

(defn compile-element-macro
  [env tag typeid body]
  (let [compile-form (partial compile-form env)
        {key :key
         {willUpdate :willUpdate willUnmount :willUnmount
          didMount :didMount didUpdate :didUpdate} :hooks :as attrs} (attributes body)
        _ (validate-attributes attrs)
        body (body-without-attributes body attrs)]
    (with-svg-namespace tag
      `((open ~tag ~typeid ~key ~willUpdate ~willUnmount)
        ~@(attribute-calls env tag attrs)
        ~@(map compile-form body)
        (close ~didMount ~didUpdate)))))

(defmacro text [& text]
  `(muance.core/text-node (cljs.core/str ~@text)))

(defn with-macro-meta [tag]
  (with-meta tag (assoc (meta tag) ::tag (str tag))))

(defmacro make-element-macro [tag]
  `(defmacro ~(with-macro-meta tag) [~'& ~'body]
     (swap! typeid inc-typeid)
     (compile-element-macro ~'&env ~(str tag) @typeid ~'body)))

(defn params-with-props [params]
  (cond (symbol? params) [params params]
        (vector? params) (let [props-sym (gensym "props")]
                           [(conj params :as props-sym) props-sym])
        (map? params) (let [props-sym (gensym "props")]
                        [(conj params [:as props-sym]) props-sym])
        :else nil))

(defmacro defcomp [name docstring-or-params & params-body]
  (swap! typeid inc-typeid)
  (let [typeid @typeid
        name (if (string? docstring-or-params)
               (vary-meta name assoc :doc docstring-or-params)
               name)
        name (vary-meta name assoc ::component true)
        params (if (string? docstring-or-params) (first params-body) docstring-or-params)
        _ (assert (<= (count params) 1)
                  (str ana/*cljs-ns* "/" name " must take 0 or 1 parameter"))
        [params-with-props props-sym] (params-with-props (first params))
        body (if (string? docstring-or-params) (rest params-body) params-body)
        key-sym (gensym "key")]
    `(defn ~name
       ~(if params-with-props
          `([~props-sym]
            (~name nil ~props-sym))
          `([]
            (~name nil)))
       (~(if params-with-props [key-sym params-with-props] [key-sym])
        (cljs.core/let [parent-props# *props*
                        parent-state-ref# *state-ref*
                        hooks# (goog.object/get ~name hooks-key)]
          (open-comp ~typeid ~(boolean params-with-props)
                     ~(when params-with-props props-sym)
                     ~name ~key-sym hooks#)
          (cljs.core/when-not *skip*
            ~@body)
          (close-comp parent-props# parent-state-ref# hooks#))))))

(defmacro hooks [comp hooks-map]
  (let [_ (assert (map? hooks-map))
        {willUpdate :willUpdate willUnmount :willUnmount
         didMount :didMount didUpdate :didUpdate
         willReceiveProps :willReceiveProps
         getInitialState :getInitialState :as attrs} hooks-map]
    `(goog.object/set
      ~comp
      hooks-key
      (cljs.core/array ~getInitialState ~willReceiveProps
                       ~didMount ~didUpdate ~willUnmount ~willUpdate))))


