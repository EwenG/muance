(ns muance.core
  (:refer-clojure :exclude [class])
  (:require [cljs.analyzer :as ana])
  (:import [cljs.tagged_literals JSValue]))

(defonce typeid (atom 0))
(def element-macros #{'a 'abbr 'acronym 'address 'div 'p})

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
                (= #'text clj-var) `(muance.core/text-node ~form)
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
  (cond (string? x) true
        (keyword? x) true
        (number? x) true
        (and (seq? x) (= (first x) `quote)) true
        (vector? x) (every? static? x)
        (map? x) (every? static? x)
        (symbol? x) (static-symbol? env x)
        :else false))

(defn as-str [x]
  (cond (string? x) x
        (keyword? x) (name x)
        :else `(cljs.core/str ~x)))

(defn attributes [body]
  (let [attrs (->> (partition 2 body)
                   (take-while (comp keyword? first)))]
    (when (not (empty? attrs))
      (assert (apply distinct? (map first attrs))
              (str "duplicate attributes: " (pr-str (map first attrs)))))
    (into {} (map vec attrs))))

(defn validate-attributes [{:keys [hooks styles on] :as attributes}]
  (when (contains? attributes :hooks) (assert (map? hooks)))
  (when (contains? attributes :styles) (assert (map? styles)))
  (when (contains? attributes :on) (assert (map? on))))

(defn validate-comp-attributes [{:keys [hooks] :as attributes}]
  (when (contains? attributes :hooks) (assert (map? hooks))))

(defn body-without-attributes [body attributes]
  (drop (* 2 (count attributes)) body))

(defn class-call [env class]
  (if (vector? class)
    (if (every? (partial static? env) class)
      (reduce #(if (nil? %1) (str %2) (str %1 " " %2)) nil class)
      (let [classes-with-spaces (-> class (interleave (repeat " ")) butlast)]
        `(attr "class" (cljs.core/str ~@classes-with-spaces))))
    (if (static? env class)
      `(attr-static "class" ~class)
      `(attr "class" ~class))))

(defn style-calls [env styles]
  (map (fn [[k v]]
         (if (static? env v)
           `(style-static ~(as-str k) ~v)
           `(style ~(as-str k) ~v)))
       styles))

(defn on-calls [env ons]
  (map (fn [[k v]]
         (cond (vector? v)
               (if (every? (partial static? env) v)
                 `(on-static ~(as-str k) (cljs.core/array ~@v))
                 `(on ~(as-str k) (cljs.core/array ~@v)))
               (and (instance? JSValue v) (vector? (.-val v)))
               (if (every? (partial static? env) v)
                 `(on-static ~(as-str k) (cljs.core/array ~@(.-val v)))
                 `(on ~(as-str k) (cljs.core/array ~@(.-val v))))
               :else (if (static? env v)
                       `(on-static ~(as-str k) ~v)
                       `(on ~(as-str k) ~v))))
       ons))

(defn attribute-calls [env attrs]
  (reduce (fn [calls [k v]]
            (case k
              :key calls
              :hooks calls
              :class (conj calls (class-call env v))
              :styles (into calls (style-calls env v))
              :on (into calls (on-calls env v))
              (conj calls (if (static? env v)
                            `(attr-static ~(as-str k) ~v)
                            `(attr ~(as-str k) ~v)))))
          '() attrs))

(defn compile-element-macro
  [env tag typeid body]
  (let [compile-form (partial compile-form env)
        {key :key
         {willUpdate :willUpdate willUnmount :willUnmount
          didMount :didMount didUpdate :didUpdate} :hooks :as attrs} (attributes body)
        _ (validate-attributes attrs)
        body (body-without-attributes body attrs)]
    (if (or key willUpdate willUnmount didMount didUpdate)
      `(do (open-opts ~tag ~typeid ~key ~willUpdate ~willUnmount)
           ~@(attribute-calls env attrs)
           ~@(map compile-form body)
           (close-opts ~didMount ~didUpdate))
      `(do (open ~tag ~typeid)
           ~@(attribute-calls env attrs)
           ~@(map compile-form body)
           (close)))))

(defmacro text [& text]
  `(muance.core/text-node (cljs.core/str ~@text)))

(defn with-macro-meta [tag]
  (with-meta tag (assoc (meta tag) ::tag (str tag))))

(defmacro make-element-macro [tag]
  `(defmacro ~(with-macro-meta tag) [~'& ~'body]
     (swap! typeid inc-typeid)
     (compile-element-macro ~'&env ~(str tag) @typeid ~'body)))

(defmacro def-element-macros []
  `(do
     ~@(for [tag element-macros]
         `(make-element-macro ~tag))))

(def-element-macros)

(defn params-with-props [params]
  (cond (symbol? params) [params params]
        (vector? params) (let [props-sym (gensym "props")]
                           [(conj params :as props-sym) props-sym])
        (map? params) (let [props-sym (gensym "props")]
                        [(conj params :as props-sym) props-sym])
        :else nil))

(defmacro defcomp [name docstring-or-params & params-body]
  (swap! typeid inc-typeid)
  (let [typeid (str @typeid)
        name (if (string? docstring-or-params)
               (vary-meta name assoc :doc docstring-or-params)
               name)
        params (if (string? docstring-or-params) (first params-body) docstring-or-params)
        _ (assert (<= (count params) 1) (str name " must take 0 or 1 argument"))
        [params-with-props props-sym] (params-with-props (first params))
        body (if (string? docstring-or-params) (rest params-body) params-body)
        {key :key
         {willUpdate :willUpdate willUnmount :willUnmount
          didMount :didMount didUpdate :didUpdate
          willReceiveProps :willReceiveProps
          getInitialState :getInitialState} :hooks :as attrs} (attributes body)
        _ (validate-comp-attributes attrs)
        body (body-without-attributes body attrs)]
    `(defn ~name
       ~(if params-with-props
          `([~params-with-props]
            (~name nil ~params-with-props))
          `([]
            (~name nil)))
       ~(if params-with-props
          `([key# ~params-with-props]
            (cljs.core/let [parent-props# *props*
                            parent-state-ref# *state-ref*]
              ~(if (or key willUpdate willUnmount didMount didUpdate
                       willReceiveProps getInitialState)
                 `(open-comp-opts ~typeid ~key true ~props-sym ~name
                                  ~willUpdate ~willUnmount
                                  ~willReceiveProps ~getInitialState)
                 `(open-comp ~typeid true ~props-sym ~name))
              (cljs.core/when-not *skip*
                ~@body
                ~(if (or key willUpdate willUnmount didMount didUpdate
                         willReceiveProps getInitialState)
                   `(close-comp-opts ~didMount ~didUpdate)
                   `(close-comp)))
              (set! *skip* false)
              (set! *props* parent-props#)
              (set! *state-ref* parent-state-ref#)
              (set! *state* (when parent-state-ref# (cljs.core/deref parent-state-ref#)))))
          `([key#]
            (cljs.core/let [parent-props# *props*
                            parent-state-ref# *state-ref*]
              ~(if (or key willUpdate willUnmount didMount didUpdate
                         willReceiveProps getInitialState)
                 `(open-comp-opts ~typeid ~key false nil ~name
                                  ~willUpdate ~willUnmount
                                  ~willReceiveProps ~getInitialState)
                 `(open-comp ~typeid false nil ~name))
              (cljs.core/when-not *skip*
                ~@body
                ~(if (or key willUpdate willUnmount didMount didUpdate
                         willReceiveProps getInitialState)
                   `(close-comp-opts ~didMount ~didUpdate)
                   `(close-comp)))
              (set! *skip* false)
              (set! *props* parent-props#)
              (set! *state-ref* parent-state-ref#)
              (set! *state* (when parent-state-ref# (cljs.core/deref parent-state-ref#)))))))))

(comment
  (macroexpand '(def-element-macros))
  )
