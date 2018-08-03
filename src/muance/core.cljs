(ns muance.core
  (:refer-clojure :exclude [remove key])
  (:require [muance.objects :as o]
            [muance.arrays :as a]
            [muance.diff :as diff]))

(defprotocol VTree
  (remove [this]
    "Detach the root node of a vtree. A detached vtree can still be patched and added back to the DOM."))

(defprotocol VTreeInsert
  (insert-before [ref-node vtree]
    "Inserts the root node of a VTree before the ref-node")
  (append-child [parent-node vtree]
    "Inserts the root node of a vtree as the last child(ren) of a parent-node."))

(defn refresh-roots []
  (o/forEach diff/roots diff/refresh-root))

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  [vnode]
  (diff/component-name vnode))

(defn key
  "Returns the :muance.core/key attribute of vnode, as a string."
  [vnode]
  (assert vnode "muance.core/key expects a vnode.")
  (a/aget vnode diff/index-key))

(defn post-render
  "Registers a function to be executed after the next Muance render pass. Takes a vnode or vtree,
  the function to be executed and up to three optional parameters to be passed to the 
  function f."
  ([vnode f]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (diff/get-render-queue vnode)
       (a/aget diff/index-render-queue-post-render)
       (a/add #js [f])))
  ([vnode f arg1]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (diff/get-render-queue vnode)
       (a/aget diff/index-render-queue-post-render)
       (a/add #js [f arg1])))
  ([vnode f arg1 arg2]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (diff/get-render-queue vnode)
       (a/aget diff/index-render-queue-post-render)
       (a/add #js [f arg1 arg2])))
  ([vnode f arg1 arg2 arg3]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (diff/get-render-queue vnode)
       (a/aget diff/index-render-queue-post-render)
       (a/add #js [f arg1 arg2 arg3]))))

(defn dom-nodes
  "Return a vector of all the DOM nodes associated with vnode."
  [vnode]
  (assert vnode "muance.core/dom-nodes expects a vnode.")
  (diff/dom-nodes vnode))

(defn dom-node
  "Return the DOM nodes associated with vnode. Returns the first children of vnode if vnode is
  a component and is associated with multiple DOM nodes."
  [vnode]
  (assert vnode "muance.core/dom-node expects a vnode.")
  (diff/dom-node vnode))

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
