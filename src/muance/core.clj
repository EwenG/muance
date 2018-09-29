(ns muance.core)

(defonce ^:private comp-typeid (atom -1))

(defn- dec-comp-typeid [t]
     ;; MIN_SAFE_INTEGER
     (if (= t -9007199254740991) -1 (dec t)))

(defn- params-with-props [params]
  (cond (symbol? params) [params params]
        (vector? params) (let [as-index (.indexOf ^java.util.List params :as)
                               props-sym (if (not= -1 as-index)
                                           (clojure.core/get params (inc as-index))
                                           (gensym "props"))
                               params (if (= -1 as-index) (conj params :as props-sym) params)]
                           [params props-sym])
        (map? params) (let [props-sym (or (:as params) (gensym "props"))
                            params (if (:as params) params (conj params [:as props-sym]))]
                        [params props-sym])
        :else nil))


(comment
  (params-with-props '[1 2 :as kk])
  (params-with-props '{e :e :as ff})
  )

;; The comp-fn does not need to be a var in order to support reloading because of the level
;; of indirection introduced by variadic arity functions
;; Although it would be better for comp-fn to be a var to avoid relying on clojurescript inernals
(defmacro defcomp
  "Define a Muance stateful component. A Muance component takes zero or one argument."
  [name docstring-or-hooks-or-params & body]
  (swap! comp-typeid dec-comp-typeid)
  (let [env-ns (do
                 (require 'cljs.analyzer)
                 @(resolve 'cljs.analyzer/*cljs-ns*))
        typeid @comp-typeid
        name (if (string? docstring-or-hooks-or-params)
               (vary-meta name assoc :doc docstring-or-hooks-or-params)
               name)
        name (vary-meta name assoc ::component typeid)
        hooks-or-params (if (string? docstring-or-hooks-or-params)
                          (first body)
                          docstring-or-hooks-or-params)
        body (if (string? docstring-or-hooks-or-params)
               (rest body)
               body)
        hooks (when (= ::hooks hooks-or-params)
                (first body))
        _ (when hooks (assert (map? hooks)))
        {will-update :will-update will-unmount :will-unmount
         remove-hook :remove-hook
         did-mount :did-mount did-update :did-update
         get-initial-state :get-initial-state
         will-receive-props :will-receive-props
         will-mount :will-mount} hooks
        params (if (= ::hooks hooks-or-params)
                 (second body)
                 hooks-or-params)
        body (if (= ::hooks hooks-or-params)
               (drop 2 body)
               body)
        _ (assert (<= (count params) 1)
                  (str env-ns "/" name " must take 0 or 1 parameter"))
        [params-with-props props-sym] (params-with-props (first params))
        key-sym (gensym "key")]
    `(defn ~name
       ~(if params-with-props
          `([~props-sym]
            (~name nil ~props-sym))
          `([]
            (~name nil)))
       (~(if params-with-props [key-sym params-with-props] [key-sym])
        (~'cljs.core/let
         [parent-component# muance.diff/*component*]
         (muance.diff/open-comp ~(str env-ns "/" name)
                                ~typeid ~(boolean params-with-props)
                                ~(when params-with-props props-sym)
                                (var ~name) ~key-sym
                                ~will-update ~will-unmount ~remove-hook
                                ~will-receive-props ~get-initial-state
                                ~will-mount)
         (~'cljs.core/when-not muance.diff/*skip*
          ~@body)
         (muance.diff/close-comp parent-component# ~did-mount ~did-update))))))
