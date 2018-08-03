(ns muance.core
  (:refer-clojure :exclude [remove key])
  (:require [cljs.repl]
            [muance.diff :as diff]
            [muance.objects :as o]))

#?(:clj
   (defn cljs-env?
     "Take the &env from a macro, and tell whether we are expanding into cljs."
     [env]
     (contains? env :ns)))

#?(:clj
   (defonce ^:private comp-typeid (atom -1)))

#?(:clj
   (defn- dec-comp-typeid [t]
     ;; MIN_SAFE_INTEGER
     (if (= t -9007199254740991) -1 (dec t))))

#?(:clj
   (defn- params-with-props [params]
     (cond (symbol? params) [params params]
           (vector? params) (let [as-index (.indexOf params :as)
                                  props-sym (if (not= -1 as-index)
                                              (get params (inc as-index))
                                              (gensym "props"))
                                  params (if (= -1 as-index) (conj params :as props-sym) params)]
                              [params props-sym])
           (map? params) (let [props-sym (or (:as params) (gensym "props"))
                               params (if (:as params) params (conj params [:as props-sym]))]
                           [params props-sym])
           :else nil)))

(comment
  (params-with-props '[1 2 :as kk])
  (params-with-props '{e :e :as ff})
  )

;; The comp-fn does not need to be a var in order to support reloading because of the level
;; of indirection introduced by variadic arity functions
;; Although it would be better for comp-fn to be a var to avoid relying on clojurescript inernals
#?(:clj
   (defmacro defcomp
     "Define a Muance stateful component. A Muance component takes zero or one argument."
     [name docstring-or-params & params-body]
     (swap! comp-typeid dec-comp-typeid)
     (let [env-ns (if (cljs-env? &env)
                    (do
                      (require 'cljs.analyzer)
                      @(resolve 'cljs.analyzer/*cljs-ns*))
                    *ns*)
           typeid @comp-typeid
           name (if (string? docstring-or-params)
                  (vary-meta name assoc :doc docstring-or-params)
                  name)
           name (vary-meta name assoc ::component typeid)
           params (if (string? docstring-or-params) (first params-body) docstring-or-params)
           _ (assert (<= (count params) 1)
                     (str env-ns "/" name " must take 0 or 1 parameter"))
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
           (cljs.core/let [parent-component# muance.diff/*component*
                           hooks# (o/get muance.diff/comp-hooks
                                         ~(str env-ns "/" name))]
             (diff/open-comp ~(str env-ns "/" name)
                             ~typeid ~(boolean params-with-props)
                             ~(when params-with-props props-sym)
                             ~name ~key-sym hooks#)
             (cljs.core/when-not muance.diff/*skip*
               ~@body)
             (diff/close-comp parent-component# hooks#)))))))

#?(:clj
   (defmacro hooks
     "Attaches a set of lifecycle hooks to a Muance component. hooks-map must be a literal map of
  lifecycle hooks."
     [component hooks-map]
     (let [not-a-comp-msg "muance.core/hooks first parameter must be a component"
           _ (assert (symbol? component) not-a-comp-msg)
           resolved (do (require 'muance.dom)
                        ((resolve 'muance.dom/cljs-resolve) &env component))
           comp-ns (and resolved (symbol (namespace resolved)))
           comp-sym (and resolved (symbol (name resolved)))
           _ (assert (and comp-ns comp-sym) not-a-comp-msg)
           var-map (get-in @cljs.env/*compiler* [::ana/namespaces comp-ns :defs comp-sym])
           comp-id (get-in var-map [:meta ::component])]
       (assert comp-id not-a-comp-msg)
       (assert (map? hooks-map))
       (let [{will-update :will-update will-unmount :will-unmount
              remove-hook :remove-hook
              did-mount :did-mount did-update :did-update
              will-receive-props :will-receive-props
              get-initial-state :get-initial-state :as attrs} hooks-map]
         `(o/set
           muance.diff/comp-hooks
           ~(str comp-ns "/" comp-sym)
           (cljs.core/array ~comp-id
                            ~get-initial-state ~will-receive-props
                            ~did-mount ~did-update ~will-unmount
                            ~remove-hook ~will-update))))))

#?(:clj
   (defn- refresh-roots [namespaces repl-env compiler-env ns-updated?]
     (loop [namespaces namespaces]
       (when-let [n (first namespaces)]
         (if (ns-updated? n)
           (cljs.repl/-evaluate
            repl-env "<cljs repl>" 1
            "muance.core.refresh_roots();")
           (recur (rest namespaces)))))))

#?(:clj
   (defn namespaces-starting-with [ns-start-sym]
     (for [[k v] (:cljs.analyzer/namespaces @cljs.env/*compiler*)
           :when (.startsWith ^String (str k) (str ns-start-sym))]
       k)))

#?(:clj
   (defmacro re-render-on-update [ns-start-sym]
     (when (-> @cljs.env/*compiler* :options :optimizations (= :none))
       (let [namespaces (namespaces-starting-with ns-start-sym)]
         (swap! cljs.env/*compiler* assoc-in
                [:replique/ns-watches (str "muance-re-render-" ns-start-sym)]
                (partial refresh-roots namespaces))))
     nil))

(defprotocol VTree
  (remove [this]
    "Detach the root node of a vtree. A detached vtree can still be patched and added back to the DOM."))

(defn patch
  "Patch a vtree using component. The optional third argument is the component props."
  ([vtree component]
   (diff/patch-root-impl vtree component diff/no-props-flag))
  ([vtree component props]
   (diff/patch-root-impl vtree component props)))

(defn state []
  (assert (not (nil? diff/*vnode*)) (str "muance.core/state was called outside of render loop"))
  diff/*state*)

(defn vnode []
  (assert (not (nil? diff/*vnode*)) (str "muance.core/vnode was called outside of render loop"))
  diff/*vnode*)

(defprotocol VTreeInsert
  (insert-before [ref-node vtree]
    "Inserts the root node of a VTree before the ref-node")
  (append-child [parent-node vtree]
    "Inserts the root node of a vtree as the last child(ren) of a parent-node."))


