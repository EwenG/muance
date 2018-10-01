(ns muance.core
  (:refer-clojure :exclude [remove key get set])
  (:require [muance.diff]
            [muance.vtree :as vtree]))

(defprotocol VTree
  (remove [this]
    "Detach the root node of a vtree. A detached vtree can still be patched and added back to the DOM.")
  (unmount [this]
    "Unmount the top level vnode of this vtree.")
  (refresh [this id]))

(defprotocol VTreeInsert
  (insert-before [ref-node vtree]
    "Inserts the root node of a VTree before the ref-node")
  (append-child [parent-node vtree]
    "Inserts the root node of a vtree as the last child(ren) of a parent-node."))

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  []
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/component-name was called outside of render loop"))
  (muance.diff/component-name muance.diff/*vnode*))

(defn key
  "Returns the :muance.core/key attribute of vnode, as a string."
  []
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/key was called outside of render loop"))
  (aget muance.diff/*vnode* muance.diff/index-key))

;; Useful to systematically execute an action on the DOM after it has been updated.
(defn post-render
  "Registers a function to be executed after the next Muance render pass."
  [f]
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/post-render was called outside of render loop"))
  (-> (muance.diff/get-render-queue muance.diff/*vnode*)
      (aget muance.diff/index-render-queue-post-render-hooks)
      (.add f)))

(defmacro with-post-render
  "Executes the function f after a muance render pass triggered by a call to muance.core/patch or a local state mutation contained in body."
  [f & body]
  `(binding [muance.diff/*post-render-fn* ~f]
     ~@body))

(defn nodes
  "Return a vector of all the real nodes associated with the current virtual node/component."
  []
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/nodes was called outside of render loop"))
  (muance.diff/nodes muance.diff/*vnode*))

(defn node
  "Return the real node associated with the current virtual node/component. If the current component has multiple real nodes, then only the first is returned."
  []
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/node was called outside of render loop"))
  (muance.diff/node muance.diff/*vnode*))

(defn parent-node
  "Return the real node associated with the parent of the current virtual node/component."
  []
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/parent-node was called outside of render loop"))
  (when-let [p-vnode (aget muance.diff/*vnode* muance.diff/index-parent-vnode)]
    (muance.diff/parent-node p-vnode)))

(defn get
  "Get a value from the ones set on the current node/component by the muance.core/set function. This function is intended to be used in lifecycle hooks."
  [k]
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/get was called outside of render loop"))
  (muance.diff/get-user-data k))

(defn set
  "Store a value on the current node/component. This function is intended to be used in lifecycle hooks. The stored value can be retrieved using muance.core/get."
  [k v]
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/set was called outside of render loop"))
  (muance.diff/set-user-data k v))

(defn unset
  "Remove a value from the current node/component. This function is intended to be used in lifecycle hooks."
  [k]
  (assert (not (nil? muance.diff/*vnode*))
          (str "muance.core/set was called outside of render loop"))
  (muance.diff/unset-user-data k))

;;;;

(defn patch
  "Patch a vtree using component. The optional third argument is the component props."
  ([vtree component]
   (patch vtree component muance.diff/no-props-flag))
  ([vtree component props]
   (let [render-queue (vtree/render-queue vtree)
         render-queue-fn (aget render-queue muance.diff/index-render-queue-fn)]
     (render-queue-fn #js [(vtree/render-queue vtree) props component
                           (vtree/vnode vtree) -1 muance.diff/*post-render-fn*]))))

(defn state []
  (assert (not (nil? muance.diff/*vnode*)) (str "muance.core/state was called outside of render loop"))
  muance.diff/*state*)

;; Most public API are required to be called in the render-loop in order to be called on the rendering thread and to avoid sharing mutable data accross threads
