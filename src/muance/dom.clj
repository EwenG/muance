(ns muance.dom
  (:require [clojure.string :as string]
            [muance.core :as m]
            [muance.attributes :as attributes]
            [cljs.env]))

(defonce ^:private typeid (atom 1))

(defn- inc-typeid [t]
  ;; MAX_SAFE_INTEGER
  (if (= t 9007199254740991) 1 (inc t)))

(defn- safe-symbol [x]
  (when x (symbol x)))

(defn- resolve-namespace
  [env sym ns]
  (if (= 'cljs.core sym)
    sym
    (-> (merge (:requires ns) (:require-macros ns))
        (get sym))))

(defn cljs-resolve [env sym]
  (when (and sym (not (get-in env [:locals sym])))
    (let [sym-ns (safe-symbol (namespace sym))
          ns-name (if sym-ns
                    (resolve-namespace env sym-ns (:ns env))
                    (get-in env [:ns :name]))]
      (symbol (str ns-name) (name sym)))))

(declare compile-element-macro)
(declare text)

(defn- compile-form [env form]
  (cond (and (seq? form) (symbol? (first form)))
        (let [var (cljs-resolve env (first form))
              clj-var (resolve var)]
          (cond (::m/tag (meta clj-var))
                (compile-element-macro env (::m/tag (meta clj-var)) nil (rest form))
                (= #'text clj-var) `(~'muance.internal.dom/text-node (cljs.core/str ~@(rest form)))
                :else form))
        (string? form) `(~'muance.internal.dom/text-node ~form)
        :else form))

;; Not used anymore. We use a simplified, less precise logic now
#_(defn- local-dep [{name :name fn-var :fn-var
                   local :local init :init
                   op :op tag :tag dynamic :dynamic
                   ns :ns :as info}]
  (cond
    ;; fn param
    (and (= :var op) (not local) (not fn-var))
    info
    ;; dynamic var
    dynamic
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

(defn- static-symbol? [env s]
  (not (contains? (:locals env) s)))

(defn- static? [env x]
  (cond (nil? x) true
        (true? x) true
        (false? x) true
        (string? x) true
        (keyword? x) true
        (number? x) true
        (and (seq? x) (= (first x) 'quote)) true
        (vector? x) (every? (partial static? env) x)
        (map? x) (every? (partial static? env) x)
        (symbol? x) (static-symbol? env x)
        :else false))

(defn- as-str [x]
  (cond (string? x) x
        (keyword? x) (name x)
        :else `(cljs.core/str ~x)))

(defn- class-call [env class]
  (if (vector? class)
    (if (every? (partial static? env) class)
      (if (some symbol? class)
        (let [classes-with-spaces (-> class (interleave (repeat " ")) butlast)]
          `(~'muance.internal.dom/prop-static "className" (cljs.core/str ~@classes-with-spaces)))
        `(~'muance.internal.dom/prop-static
          "className" ~(reduce #(if (nil? %1) (str %2) (str %1 " " %2)) nil class)))
      (let [classes-with-spaces (-> class (interleave (repeat " ")) butlast)]
        `(~'muance.internal.dom/prop "className" (cljs.core/str ~@classes-with-spaces))))
    (if (static? env class)
      `(~'muance.internal.dom/prop-static "className" ~class)
      `(~'muance.internal.dom/prop "className" ~class))))

(defn- style-calls [env style]
  (map (fn [[k v]]
         (if (string/starts-with? (str k) ":--")
           (if (static? env v)
             `(~'muance.internal.dom/style-custom-static ~(as-str k) ~v)
             `(~'muance.internal.dom/style-custom ~(as-str k) ~v))
           (if (static? env v)
             `(~'muance.internal.dom/style-static ~(as-str k) ~v)
             `(~'muance.internal.dom/style ~(as-str k) ~v))))
       style))

;; Wrap event handlers in a var when in dev compilation mode in order to enable the possibility
;; to dynamically redefine event handler functions definition
(defn- maybe-wrap-in-var [env f]
  (if (and (-> @cljs.env/*compiler* :options :optimizations (= :none)) (symbol? f))
    (if-let [resolved (cljs-resolve env f)]
      `(cljs.core/Var. (cljs.core/fn [] ~f) (quote ~resolved) nil)
      f)
    f))

(defn- on-calls [env ons]
  (let [static? (partial static? env)
        ons (if (attributes/handler? ons) [ons] ons)]
    (map (fn [[k f & args]]
           (if (and (static? f) (every? static? args))
             (let [l (count args)]
               (cond (= 0 l) `(~'muance.internal.dom/on-static-0 ~(as-str k) ~(maybe-wrap-in-var env f))
                     (= 1 l) `(~'muance.internal.dom/on-static-1 ~(as-str k) ~(maybe-wrap-in-var env f)
                               ~(nth args 0))
                     (= 2 l) `(~'muance.internal.dom/on-static-2 ~(as-str k) ~(maybe-wrap-in-var env f)
                               ~(nth args 0) ~(nth args 1))
                     :else `(~'muance.internal.dom/on-static-3 ~(as-str k) ~(maybe-wrap-in-var env f)
                             ~(nth args 0)
                             ~(nth args 1)
                             ~(nth args 2))))
             (let [l (count args)]
               (cond (= 0 l) `(~'muance.internal.dom/on-0 ~(as-str k) ~(maybe-wrap-in-var env f))
                     (= 1 l) `(~'muance.internal.dom/on-1 ~(as-str k) ~(maybe-wrap-in-var env f)
                               ~(nth args 0))
                     (= 2 l) `(~'muance.internal.dom/on-2 ~(as-str k) ~(maybe-wrap-in-var env f)
                               ~(nth args 0) ~(nth args 1))
                     :else `(~'muance.internal.dom/on-3 ~(as-str k) ~(maybe-wrap-in-var env f)
                             ~(nth args 0)
                             ~(nth args 1)
                             ~(nth args 2))))))
         ons)))

(defn- attribute-calls [env tag attrs]
  (reduce (fn [calls [k v]]
            (cond
              (= k ::m/key) calls
              (= k ::m/hooks) calls
              (= k :className) (conj calls (class-call env v))
              (= k :style) (into calls (style-calls env v))
              (= k ::m/on)  (into calls (on-calls env v))
              (and (= tag "input") (= k :value))
              (conj calls (if (static? env v)
                            `(~'muance.internal.dom/prop-static "value" ~v)
                            `(~'muance.internal.dom/input-value ~v)))
              (string/starts-with? (str k) ":xlink")
              (conj calls (if (static? env v)
                            `(~'muance.internal.dom/attr-ns-static muance.dom/xlink-ns ~(as-str k) ~v)
                            `(~'muance.internal.dom/attr-ns muance.dom/xlink-ns ~(as-str k) ~v)))
              (string/starts-with? (str k) ":xml")
              (conj calls (if (static? env v)
                            `(~'muance.internal.dom/attr-ns-static muance.dom/xml-ns ~(as-str k) ~v)
                            `(~'muance.internal.dom/attr-ns muance.dom/xml-ns ~(as-str k) ~v)))
              (string/starts-with? (str k) ":data-")
              (conj calls (if (static? env v)
                            `(~'muance.internal.dom/attr-ns-static nil ~(as-str k) ~v)
                            `(~'muance.internal.dom/attr-ns nil ~(as-str k) ~v)))
              (string/starts-with? (str k) ":aria-")
              (conj calls (if (static? env v)
                            `(~'muance.internal.dom/attr-ns-static nil ~(as-str k) ~v)
                            `(~'muance.internal.dom/attr-ns nil ~(as-str k) ~v)))
              (= "muance.attribute" (namespace k))
              (conj calls (if (static? env v)
                            `(~'muance.internal.dom/attr-ns-static nil ~(as-str k) ~v)
                            `(~'muance.internal.dom/attr-ns nil ~(as-str k) ~v)))
              :else (conj calls (if (static? env v)
                                  `(~'muance.internal.dom/prop-static ~(as-str k) ~v)
                                  `(~'muance.internal.dom/prop ~(as-str k) ~v)))))
          [] attrs))

(defn- with-svg-namespace [tag body]
  (case tag
    "svg" `(do
             (set! muance.diff/*svg-namespace* (cljs.core/inc muance.diff/*svg-namespace*))
             ~@body
             (set! muance.diff/*svg-namespace* (cljs.core/dec muance.diff/*svg-namespace*)))
    ;; *svg-namespace* is set to 0 in open-impl
    "foreignObject" `(let [parent-svg-namespace# muance.diff/*svg-namespace*]
                       ~@body
                       (set! muance.diff/*svg-namespace* parent-svg-namespace#))
    `(do ~@body)))

(defn compile-element-macro
  [env tag typeid body]
  (let [compile-form (partial compile-form env)
        {key ::m/key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update
          will-mount :will-mount
          get-initial-state :get-initial-state} ::m/hooks
         :as attrs} (attributes/attributes body)
        _ (attributes/validate-attributes attrs)
        body (attributes/body-without-attributes body attrs)]
    (with-svg-namespace tag
      `((muance.diff/open ~tag ~typeid ~key ~will-update ~will-unmount ~will-mount
                          ~get-initial-state ~remove-hook)
        ~@(attribute-calls env tag attrs)
        ~@(doall (map compile-form body))
        (muance.diff/close ~did-mount ~did-update)))))

(defmacro text
  "Creates a text node. The text node value is the string concatenation of the text macro 
  arguments."
  [& text]
  `(~'muance.internal.dom/text-node (cljs.core/str ~@text)))

(defmacro make-element-macro
  "Defines a new HTML element macro with the provided tag. The newly defined HTML element macro
  can be used during a Muance vtree patching to create an HTML element which name is the provided
  tag."
  [tag]
  `(defmacro ~(vary-meta tag assoc ::m/tag (str tag)) [~'& ~'body]
     (swap! @#'typeid #'inc-typeid)
     (compile-element-macro ~'&env ~(str tag) @@#'typeid ~'body)))




