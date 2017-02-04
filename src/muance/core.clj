(ns muance.core
  (:refer-clojure :exclude [class])
  (:import [cljs.tagged_literals JSValue]))

(defonce typeid (atom 0))
(def element-macros #{'a 'abbr 'acronym 'address 'div 'p})

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
(declare compile-attrs)
(declare compile-classes)
(declare compile-styles)
(declare attrs)
(declare class)
(declare style)

(defn compile-form [env form]
  (cond (and (seq? form) (symbol? (first form)))
        (let [var (cljs-resolve env (first form))
              clj-var (resolve var)]
          (cond (::tag (meta clj-var))
                (compile-element-macro env (::tag (meta clj-var)) (rest form))
                (= #'attrs clj-var) (compile-attrs env form)
                (= #'class clj-var) (compile-classes env form)
                (= #'style clj-var) (compile-styles env form)
                (= 'cljs.core/str var) `(~'text ~form)
                :else form))
        (string? form) `(~'text ~form)
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

(defmacro attrs [& attributes]
  (cond (empty? attributes)
        nil
        (not (even? (count attributes)))
        (throw (IllegalArgumentException. "attrs expects an even number of forms"))
        :else (let [compiled-attrs (for [[k v] (partition 2 attributes)]
                                     (let [k (if (keyword? k) (name k) k)]
                                       `(attr ~k ~v)))]
                (cons 'do compiled-attrs))))

(defn compile-attrs [env [m & attributes]]
  (cond (empty? attributes)
        nil
        (not (even? (count attributes)))
        (throw (IllegalArgumentException. "attrs expects an even number of forms"))
        :else (let [compiled-attrs (for [[k v] (partition 2 attributes)]
                                     (let [k (if (keyword? k) (name k) k)]
                                       (if (and (static? env k) (static? env v))
                                         `(attr-static ~k ~v)
                                         `(attr ~k ~v))))]
                (cons 'do compiled-attrs))))

(defmacro class [& classes]
  (if (empty? classes)
    nil
    (let [class-fn (cond (= 1 (count classes)) `class1
                         (= 2 (count classes)) `class2
                         (= 3 (count classes)) `class3
                         :else `class)]
      `(~class-fn ~@classes))))

(defn compile-classes [env & classes]
  (if (empty? classes)
    nil
    (let [static? (every? (partial static? env) classes)
          class-fn (cond (and static? (= 1 (count classes))) `class-static1
                         (= 1 (count classes)) `class1
                         (and static? (= 2 (count classes))) `class-static2
                         (= 2 (count classes)) `class-static2
                         (and static? (= 3 (count classes))) `class-static3
                         (= 3 (count classes)) `class3
                         static? `class-static
                         :else `class)]
      `(~class-fn ~@classes))))

(defmacro style [& styles]
  (cond (empty? styles)
        nil
        (not (even? (count styles)))
        (throw (IllegalArgumentException. "style expects an even number of forms"))
        :else (let [compiled-attrs (for [[k v] (partition 2 styles)]
                                     (let [k (if (keyword? k) (name k) k)]
                                       `(style ~k ~v)))]
                (cons 'do compiled-attrs))))

(defn compile-style [env [m & styles]]
  (cond (empty? styles)
        nil
        (not (even? (count styles)))
        (throw (IllegalArgumentException. "style expects an even number of forms"))
        :else (let [compiled-attrs (for [[k v] (partition 2 styles)]
                                     (let [k (if (keyword? k) (name k) k)]
                                       (if (and (static? env k) (static? env v))
                                         `(style-static ~k ~v)
                                         `(style ~k ~v))))]
                (cons 'do compiled-attrs))))

(defn lifecycle-as-map [lifecycle]
  (if (instance? JSValue lifecycle)
    (.-val lifecycle)
    {:didMount `(goog.object/get ~lifecycle "didMount")
     :willUpdate `(goog.object/get ~lifecycle "willUpdate")
     :willMove `(goog.object/get ~lifecycle "willMove")
     :didUpdate `(goog.object/get ~lifecycle "didUpdate")
     :didMove `(goog.object/get ~lifecycle "didMove")
     :willUnmount `(goog.object/get ~lifecycle "willUnmount")}))

(defn compile-element-macro
  ([env tag body]
   (let [compile-form (partial compile-form env)
         {:keys [didMount willUpdate didUpdate willUnmount] :as lifecycle}
         (cond
           (= :lifecycle (first body)) (-> body second lifecycle-as-map)
           :else nil)
         body (if lifecycle (drop 2 body) body)]
     (if lifecycle
       `(do (open-lifecycle ~tag ~willUpdate ~willUnmount)
            ~@(map compile-form body)
            (close-lifecycle ~didMount ~didUpdate))
       `(do (open ~tag)
            ~@(map compile-form body)
            (close)))))
  ([env tag typeid body]
   (let [compile-form (partial compile-form env)
         {:keys [didMount willUpdate didUpdate willUnmount] :as lifecycle}
         (cond
           (= :lifecycle (first body)) (-> body second lifecycle-as-map)
           :else nil)
         body (if lifecycle (drop 2 body) body)]
     (if lifecycle
       `(do (open-typeid-lifecycle ~tag ~typeid ~willUpdate ~willUnmount)
            ~@(map compile-form body)
            (close-lifecycle ~didMount ~didUpdate))
       `(do (open-typeid ~tag ~typeid)
            ~@(map compile-form body)
            (close))))))

(defmacro with-key [key & body]
  `(if cljs.core/*assert*
     (let [current-node# *current-vnode*
           children-count# (or (cljs.core/aget current-node# index-children-count) 0)]
       (set! *key* ~key)
       ~@body
       (set! *key* nil)
       (cljs.core/assert (cljs.core/and
                          (cljs.core/identical? current-node# *current-vnode*)
                          (cljs.core/<=
                           (cljs.core/-
                            (or (cljs.core/aget *current-vnode* index-children-count) 0)
                            children-count#) 1))
                         "with-key must enclose a single child"))
     (do (set! *key* ~key)
         ~@body
         (set! *key* nil))))

(defn with-macro-meta [tag]
  (with-meta tag (assoc (meta tag) ::tag (str tag))))

(defmacro make-element-macro [tag]
  `(defmacro ~(with-macro-meta tag) [~'& ~'body]
     (swap! typeid inc)
     (compile-element-macro ~'&env ~(str tag) @typeid ~'body)))

(defmacro def-element-macros []
  `(do
     ~@(for [tag element-macros]
         `(make-element-macro ~tag))))

(def-element-macros)

(defmacro defcomp [name docstring-or-params & params-body]
  (let [name (if (string? docstring-or-params)
               (vary-meta name assoc :doc docstring-or-params)
               name)
        params (if (string? docstring-or-params) (first params-body) docstring-or-params)
        body (if (string? docstring-or-params) (rest params-body) params-body)]
    (assert (<= (count params) 1) (str name " must take 0 or 1 argument"))
    `(defn ~name ~params ~@body)))

(comment
  (macroexpand '(def-element-macros))

  (p (p))
  
  )
