(ns muance.core
  (:refer-clojure :exclude [remove key get set])
  (:require [muance.diff :as diff]
            [muance.vtree :as vtree]
            [muance.objects :as o]
            [muance.arrays :as a])
  #?(:clj (:import [java.util ArrayList])))

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
           (vector? params) (let [as-index (.indexOf ^java.util.List params :as)
                                  props-sym (if (not= -1 as-index)
                                              (clojure.core/get params (inc as-index))
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
     [name docstring-or-hooks-or-params & body]
     (swap! comp-typeid dec-comp-typeid)
     (let [env-ns (if (cljs-env? &env)
                    (do
                      (require 'cljs.analyzer)
                      @(resolve 'cljs.analyzer/*cljs-ns*))
                    *ns*)
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
            will-receive-props :will-receive-props} hooks
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
           (~(if (cljs-env? &env)
               'cljs.core/let
               'clojure.core/let)
            [parent-component# muance.diff/*component*]
            (diff/open-comp ~(str env-ns "/" name)
                            ~typeid ~(boolean params-with-props)
                            ~(when params-with-props props-sym)
                            (var ~name) ~key-sym
                            ~will-update ~will-unmount ~remove-hook
                            ~will-receive-props ~get-initial-state)
            (~(if (cljs-env? &env)
                'cljs.core/when-not
                'clojure.core/when-not) muance.diff/*skip*
             ~@body)
            (diff/close-comp parent-component# ~did-mount ~did-update)))))))

(defprotocol VTree
  (remove [this]
    "Detach the root node of a vtree. A detached vtree can still be patched and added back to the DOM.")
  (refresh #?(:cljs [this id] :clj [this id it])))

(defprotocol VTreeInsert
  (insert-before [ref-node vtree]
    "Inserts the root node of a VTree before the ref-node")
  (append-child [parent-node vtree]
    "Inserts the root node of a vtree as the last child(ren) of a parent-node."))

(defn refresh-roots []
  (o/forEach diff/roots refresh))

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  []
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/component-name was called outside of render loop"))
  (diff/component-name diff/*vnode*))

(defn key
  "Returns the :muance.core/key attribute of vnode, as a string."
  []
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/key was called outside of render loop"))
  (a/aget diff/*vnode* diff/index-key))

;; Useful to systematically execute an action on the DOM after it has been updated, or execute
;; and action after a state update from a handler. 
(defn post-render
  "Registers a function to be executed after the next Muance render pass."
  [f]
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/post-render was called outside of render loop"))
  (-> (diff/get-render-queue diff/*vnode*)
      (a/aget diff/index-render-queue-post-render-hooks)
      (a/add f)))

(defmacro with-post-render
  "Executes the function f after a muance render pass triggered by a call to muance.core/patch or a local state mutation contained in body."
  [f & body]
  `(binding [diff/*post-render-fn* ~f]
     ~@body))

(defn nodes
  "Return a vector of all the real nodes associated with the current virtual node/component."
  []
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/nodes was called outside of render loop"))
  (diff/nodes diff/*vnode*))

(defn node
  "Return the real node associated with the current virtual node/component. If the current component has multiple real nodes, then only the first is returned."
  []
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/node was called outside of render loop"))
  (diff/node diff/*vnode*))

(defn parent-node
  "Return the real node associated with the parent of the current virtual node/component."
  []
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/parent-node was called outside of render loop"))
  (when-let [p-vnode (a/aget diff/*vnode* diff/index-parent-vnode)]
    (diff/parent-node p-vnode)))

(defn get
  "Get a value from the ones set on the current node/component by the muance.core/set function. This function is intended to be used in lifecycle hooks."
  [k]
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/get was called outside of render loop"))
  (when-let [user-data (a/aget diff/*vnode* diff/index-user-data)]
    (o/get user-data k)))

(defn set
  "Store a value on the current node/component. This function is intended to be used in lifecycle hooks. The stored value can be retrieved using muance.core/get."
  [k v]
  (assert (not (nil? diff/*vnode*))
          (str "muance.core/set was called outside of render loop"))
  (let [user-data (a/aget diff/*vnode* diff/index-user-data)]
    (if (nil? user-data)
      (let [user-data #?(:cljs #js {} :clj (java.util.HashMap.))]
        (o/set user-data k v)
        (a/aset diff/*vnode* diff/index-user-data user-data))
      (o/set user-data k v))))

;;;;

(defn patch
  "Patch a vtree using component. The optional third argument is the component props."
  ([vtree component]
   (patch vtree component diff/no-props-flag))
  ([vtree component props]
   (let [render-queue (vtree/render-queue vtree)
         render-queue-fn (a/aget render-queue diff/index-render-queue-fn)]
     (render-queue-fn #?(:cljs #js [(vtree/render-queue vtree) props component
                                    (vtree/vnode vtree) -1 diff/*post-render-fn*]
                         :clj (doto (ArrayList. 6)
                                (.add (vtree/render-queue vtree))
                                (.add props)
                                (.add component)
                                (.add (vtree/vnode vtree))
                                (.add -1)
                                (.add diff/*post-render-fn*)))))))

(defn state []
  (assert (not (nil? diff/*vnode*)) (str "muance.core/state was called outside of render loop"))
  diff/*state*)

;; Most public API are required to be called in the render-loop in order to be called on the rendering thread and to avoid sharing mutable data accross threads
