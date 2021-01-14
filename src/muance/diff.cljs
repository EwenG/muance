(ns muance.diff
  (:require [muance.local-state :as ls]
            [goog.object :as o]
            [muance.vtree :as vtree]
            [muance.context :as context]))

;; VNodes fields
;; - typeid
;; - parentVnode
;; - nodeOrCompData
;; - componentOrCompProps
;; - childrenCounts
;; - children
;; - attrsOrCompState
;; - userData
;; Unmount is stored on the node since it must be called when one of the parents of the node
;; is removed
;; - unmount
;; - removeHook
;; - key
;; A slot which stores one of two flags:
;;   moved-flag
;;   moving-flag
;;   new-flag
;; See the documentation for these two flags for more details
;; - keyMoved
;; Keep track of the vnode sibling in order to reorder keyed vnodes during child nodes
;; reconciliation
;; - keyNextVnode
;; - keymap
;; When a keyed node is removed, the keymap is marked as invalid. Invalid keymaps are
;; cleaned when the close function of the node is called
;; - keymapInvalid

;; - compDataName
;; - compDataStateRef
;; - compDataSvgNamespace
;; index-in-parent is used when rendering a component after its local state has changed.
;; we must initialize the children-count slot to the same value than index-in-parent
;; - compDataIndexInParent
;; the depth of the component is stored to be able to init the component-state var when a
;; component is re-rendered because of a local state change
;; - compDataDepth
;; Not used by the DOM implementation
;; - (def ^:const index-comp-data-dirty-flag 5)
;; *render-flag* is put here when the flag has been rendered. This must only be touched by the
;; rendering thread !
;; - compDataRenderedFlag

;; - renderQueueFn
;; Not used by the DOM implementation
;; - (def ^:const index-render-queue-synchronous 1)
;; Whether the rendering thread is still rendering dirty components or not. Must only be modified by the batching thread
;; Not used by the DOM implementation
;; (def ^:const index-render-queue-processing-flag 2)
;; Whether dirty comps have been enqueued by the batching thread, waiting to be processed by the rendering thread. Must only be modified by the batching thread.
;; Not used by the DOM implementation
;; (def ^:const index-render-queue-pending-flag 3)
;; A flag used to know when a component has already be enqueued by the batching thread. Used to avoid to enqueue a component twice. Must only be modified by the batching thread. This flag is set on the component data at the index-comp-data-dirty-flag index.
;; Not used by the DOM implementation
;; (def ^:const index-render-queue-dirty-flag 4)
;; Not used by the DOM implementation
;; (def ^:const index-render-queue-first-render-promise 5)
;; The post render hooks. Must only be modified by the rendering thread.
;; - postRenderHooks
;; The offset of the dirty comps in the rendering-queue. A component is enqueued by the batching thread at the index (+ component-depth index-render-queue-offset). Dirty components are reset to nil before beeing passed to the rendering thread. The dirty comps used by the rendering thread are a flat copy (not deep copy !) of the dirty comps. Thus the batching thread and the rendering thread do not share the same array. They can both mutate this array.
;; Not used by the DOM implementation
;; - (def ^:const index-render-queue-offset 7)




(def ^{:dynamic true} *component* nil)
;; Whether the current vnode has just been created or not
(def ^{:dynamic true} *new-node* nil)
(def ^{:dynamic true} *attrs-count* nil)
;; Used for two different things:
;; - Used to handle a edge case when open-impl returns two vnodes to be removed.
;; See the invalid states handling in open-impl
;; - Bound to the vnode that is going to be removed when checking for remove-hooks
;; This lets the user the possibility to prevent the real DOM node removal. The user will
;; instead get a reference to the real node to remove it later
(def ^{:dynamic true} *vnode-to-remove* nil)
(def ^{:dynamic true} *props* nil)
;; Whether to skip a component body or not, depending on whether its props and state has changed
;; or not
(def ^{:dynamic true} *skip* nil)
;; When true, components always render, even if props or state did not change
(def ^{:dynamic true} *force-render* nil)
;; component-depth is used to be able to always render components top -> down. Rendering
;; components top->down avoids unecessary diffing sometimes
(def ^{:dynamic true} *component-depth* nil)
;; Used to avoid re-rendering when a state update is done from a will-receive-props hook
(def ^{:dynamic true} *watch-local-state* true)
(def ^{:dynamic true} *components-queue-count* nil)
;; Components that need to be re-rendered are stored in the render-queue
(def ^{:dynamic true} *render-queue* nil)
;; incremented on svg open, decremented on svg close, reseted to 0 on foreignObject open,
;; previous value restored on foreignObject close
(def ^{:dynamic true} *svg-namespace* nil)
;; Hold the children property/ObservableList of the current javafx node
(def ^{:dynamic true} *children* nil)
;; Whether a remove-hook prevents the dom node from beeing removed or not
(def ^{:dynamic true} *prevent-node-removal* nil)

;; Nodes that moved because of child nodes reconciliation
;; are marked with this flag. This is useful to detect an attempt to move an already moved node,
;; a situation which can happen when duplicate keys are met.
;; This flag changes on every render pass because this helps keeping things consitent, even
;; when an exception occurs
(def ^{:dynamic true} *moved-flag* nil)
;; Nodes are marked as "moving" during child nodes reconciliation when they get removed. We mark
;; them as moving because a removed node may be a node that is in fact moving further. So we
;; mark it as moving but keep it in the keymap so it can be added back later. If it is added
;; back later (the node moved further), then we clean the moving-flag, else (if the node really
;; is a removed node) the node is really removed when cleaning the keymap
(defonce moving-flag #js [])
;; New nodes are marked as "new" during child nodes reconciliation. This is useful to be able to
;; reorder keyed nodes.
;; This flag changes on every render pass because this helps keeping things consitent, even
;; when an exception occurs
(def ^{:dynamic true} *new-flag* nil)
;; A flag used to mark a component has rendered while processing the render-queue
(def ^:dynamic *rendered-flag* nil)
;; Set on a component "props" slot when this component does not have props. This is useful to
;; differentiate between "nil" props and no props at all. When a component does not have props,
;; no props are passed to the patch function when it is re-rendered.
(defonce no-props-flag #js [])
;; Used to enqueue components with a did-mount / will-unmount hook, and then call the hooks
;; in order
(def components-queue #js [])
;; A function to be called after a muance render pass triggered by a call to muance.core/patch or a local state mutation
(def ^:dynamic *post-render-fn* nil)

(def ^{:dynamic true} *state* nil)
(def ^{:dynamic true} *vnode* nil)

(defn component? [vnode]
  (let [typeid (o/get vnode "typeid")]
    (and typeid (not (string? typeid)) (< typeid 0))))

(defn already-rendered-component? [vnode]
  (identical? *rendered-flag* (o/get (o/get vnode "nodeOrCompData") "compDataRenderedFlag")))

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  [vnode]
  (assert vnode "muance.core/component-name expects a vnode.")
  (if (component? vnode)
    (o/get (o/get vnode "nodeOrCompData") "compDataName")
    (o/get (o/get (o/get vnode "componentOrCompProps") "nodeOrCompData") "compDataName")))

(defn- nodes* [acc vnode]
  (if (component? vnode)
    (when-let [children (o/get vnode "children")]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (nodes* acc (aget children i))
            (recur (inc i))))))
    (.push acc (o/get vnode "nodeOrCompData")))
  acc)

(declare ref-node-down)

(defn nodes
  "Return a vector of all the real nodes associated with vnode."
  [vnode]
  (if (component? vnode)
    (into [] (nodes* #js [] vnode))
    [(o/get vnode "nodeOrCompData")]))

(defn node
  "Return the real nodes associated with vnode. Returns the first children of vnode if vnode is
  a component and is associated with multiple real nodes."
  [vnode]
  (if (component? vnode)
    (ref-node-down vnode)
    (o/get vnode "nodeOrCompData")))

(defn remove-vnode-key [vnode]
  (let [parent (o/get vnode "parentVnode")]
    (when-not (identical? (o/get vnode "keyMoved") *moved-flag*)
      (o/set vnode "keyMoved" moving-flag)
      (o/set parent "keymapInvalid" (inc (o/get parent "keymapInvalid"))))))

(defn on-state-change [stateful-data n]
  (when *watch-local-state*
    (let [vnode (aget stateful-data 0)
          comp-fn (aget stateful-data 1)
          render-queue (aget stateful-data 2)
          render-queue-fn (o/get render-queue "renderQueueFn")
          component-depth (o/get (o/get vnode "nodeOrCompData") "compDataDepth")]
      (render-queue-fn #js {:renderQueue render-queue
                            :props (o/get vnode "componentOrCompProps")
                            :compFn comp-fn
                            :vnode vnode
                            :depth component-depth
                            :postRenderFn *post-render-fn*}))))

(defn parent-node [parent]
  (if (component? parent)
    (recur (o/get parent "parentVnode"))
    (o/get parent "nodeOrCompData")))

(defn remove-real-node [vnode]
  (if (component? vnode)
    (when-let [children (o/get vnode "children")]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (remove-real-node (aget children i))
            (recur (inc i))))))
    (let [p-vnode (o/get vnode "parentVnode")]
      (when p-vnode
        (context/remove-node (parent-node p-vnode) (o/get vnode "nodeOrCompData"))))))

(defn enqueue-unmounts [vnode]
  (when (component? vnode)
    (context/-remove-muance-watcher
     (o/get (o/get vnode "nodeOrCompData") "compDataStateRef")))
  (when (o/get vnode "unmount")
    (aset components-queue *components-queue-count* vnode)
    (set! *components-queue-count* (inc *components-queue-count*)))
  (when (and (not *prevent-node-removal*) (not *force-render*))
    (when-let [remove-hook (o/get vnode "removeHook")]
      (set! *prevent-node-removal* true)
      (set! *vnode* vnode)
      (if (and (component? *vnode-to-remove*)
               (o/get *vnode-to-remove* "children")
               (> (.-length (o/get *vnode-to-remove* "children")) 1))
        (remove-hook (nodes *vnode-to-remove*))
        (remove-hook (node *vnode-to-remove*)))))
  (when-let [children (o/get vnode "children")]
    (let [children-count (.-length children)]
      (loop [i 0]
        (when (< i children-count)
          (let [child (aget (o/get vnode "children") i)]            
            (enqueue-unmounts child))
          (recur (inc i)))))))

(defn comp-props [vnode]
  (let [props (o/get vnode "componentOrCompProps")]
    (if (identical? props no-props-flag)
      nil props)))

(defn call-unmounts [queue-start]
  (loop [i (dec *components-queue-count*)]
    (when (>= i queue-start)
      (let [vnode (aget components-queue i)
            component (if (component? vnode) vnode (o/get vnode "componentOrCompProps"))
            props (comp-props component)
            state (o/get component "attrsOrCompState")]
        ;; *vnode* is rebound in remove-vnode
        (set! *vnode* vnode)
        ((o/get vnode "unmount") props state))
      (recur (dec i))))
  (set! *components-queue-count* queue-start))

(defn ref-node-down [vnode]
  (if (component? vnode)
    (when-let [children (o/get vnode "children")]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (if-let [node (ref-node-down (aget children i))]
              node
              (recur (inc i)))))))
    (o/get vnode "nodeOrCompData")))

;; find the vnode after the next child of the parent, or (recursively) of the grand-parent
;; if the parent is a component and does not have a next child with a dom node
(defn ref-node-up [vnode index-in-parent]
  ;; index-children has already been incremented
  ;; children cannot be nil
  (let [children (o/get vnode "children")
        l (.-length children)
        ;; if next-vnode is a keyed vnode, let's start by looking into the
        ;; next-vnode. If next-vnode is not a keyed node, it has already been removed from
        ;; the dom and thus cannot be used as a ref-node
        next-vnode (o/get vnode "keyNextVnode")
        found-node (loop [i index-in-parent
                          found-node (and next-vnode
                                          (not (nil? (o/get next-vnode "key")))
                                          (ref-node-down next-vnode))]
                     (if found-node
                       found-node
                       (when (< i l)
                         (recur (inc i) (ref-node-down (aget children i))))))]
    (if (nil? found-node)
      (when (component? vnode)
        (recur (o/get vnode "parentVnode")
               (o/get (o/get vnode "parentVnode") "childrenCount")))
      found-node)))

(defn insert-vnode-before* [parent-node vnode ref-node]
  (if (component? vnode)
    (when-let [children (o/get vnode "children")]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (insert-vnode-before* parent-node (aget children i) ref-node)
            (recur (inc i))))))
    (context/insert-before parent-node vnode ref-node)))

;; index-in-parent is passed as a parameter because, when reordering keyed nodes, the
;; index-children-count slot has already been incremented by all children
(defn insert-vnode-before [parent-vnode vnode ref-vnode index-in-parent]
  (if (component? parent-vnode)
    (let [parent-node (parent-node parent-vnode)]
      (if-let [ref-node (when ref-vnode (ref-node-down ref-vnode))]
        (insert-vnode-before* parent-node vnode ref-node)
        (insert-vnode-before*
         parent-node vnode (ref-node-up parent-vnode (inc index-in-parent)))))
    (let [parent-node (o/get parent-vnode "nodeOrCompData")]
      (if (nil? ref-vnode)
        (insert-vnode-before* parent-node vnode nil)
        (if-let [ref-node (ref-node-down ref-vnode)]
          (insert-vnode-before* parent-node vnode ref-node)
          (insert-vnode-before*
           parent-node vnode (ref-node-up parent-vnode (inc index-in-parent))))))))

(defn remove-vnode [vnode]
  (let [current-vnode *vnode*
        queue-start *components-queue-count*]
    (binding [*vnode-to-remove* vnode
              *prevent-node-removal* false]
      (enqueue-unmounts vnode)
      (call-unmounts queue-start)
      (set! *vnode* current-vnode)
      (when (not *prevent-node-removal*)
        (remove-real-node vnode)))))

(defn clean-keymap [vnode]
  (let [keymap-invalid (or (o/get vnode "keymapInvalid") 0)]
    (when (> keymap-invalid 0)
      (let [keymap (o/get vnode "keymap")]
        (o/forEach keymap
                   (fn [v k]
                     (when (identical? (o/get v "keyMoved") moving-flag)
                       (remove-vnode v)
                       (o/remove keymap k)))))
      (o/set vnode "keymapInvalid" 0))))

(defn keyed-next-vnode [vnode]
  (let [next-vnode (o/get vnode "keyNextVnode")]
    (if (and next-vnode (identical? moving-flag (o/get next-vnode "keyMoved")))
      (recur next-vnode)
      next-vnode)))

(defn reorder-nodes [parent-vnode]
  (let [keymap (o/get parent-vnode "keymap")]
    (when keymap
      (let [children (o/get parent-vnode "children")]
        (loop [i (dec (.-length children))
               next-vnode nil
               next-moved? false
               next-vnode-ref nil]
          (when (> i -1)
            (let [vnode (aget children i)
                  prev-next-vnode (keyed-next-vnode vnode)
                  moved? (cond (not (identical? prev-next-vnode next-vnode-ref))
                               (do
                                 (insert-vnode-before parent-vnode vnode next-vnode i)
                                 true)
                               (identical? (o/get vnode "keyMoved") *new-flag*)
                               true
                               :else false)]
              ;; text nodes are not set a next-vnode, thus they are moved more often than
              ;; needed but we don't really care
              (when (not= (o/get vnode "typeid") 0)
                (o/set vnode "keyNextVnode" next-vnode))
              (recur (dec i) vnode moved? (if moved? next-vnode-ref vnode)))))))))

(defn clean-children [vnode]
  (when-let [children (o/get vnode "children")]
    (let [children-count (o/get vnode "childrenCount")
          children-length (.-length children)]
      (loop [l children-length]
        (when (> l children-count)
          (let [removed-vnode (.pop children)
                k (o/get removed-vnode "key")]
            (if k
              (remove-vnode-key removed-vnode)
              (remove-vnode removed-vnode)))
          (recur (dec l)))))))

(defn init-keymap [keymap]
  (o/set *vnode* "keymap" keymap)
  (o/set *vnode* "keymapInvalid" 0)
  keymap)

(defn new-vnode [typeid element]
  #js {:typeid typeid
       :parentVnode *vnode*
       :nodeOrCompData element
       :componentOrCompProps nil
       :childrenCount nil
       :children nil
       :attrsOrCompState nil
       :userData nil
       :unmount nil
       :removeHook nil
       :key nil
       :keyMoved nil
       :keyNextVnode nil
       :keymap nil
       :keymapInvalid nil})

(defn new-vnode-key [typeid element keymap key]
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode  #js {:typeid typeid
                    :parentVnode *vnode*
                    :nodeOrCompData element
                    :componentOrCompProps nil
                    :childrenCount nil
                    :children nil
                    :attrsOrCompState nil
                    :userData nil
                    :unmount nil
                    :removeHook nil
                    :key key
                    :keyMoved *new-flag*
                    :keyNextVnode nil
                    :keymap nil
                    :keymapInvalid nil}]
    (o/set keymap key vnode)
    vnode))

(defn call-will-receive-props [prev-props props state-ref will-receive-props]
  (when (and will-receive-props (not (identical? prev-props props)))
    (will-receive-props prev-props props state-ref)
    (set! *state* @state-ref)))

;; hooks are called after open-impl to keep things consistent in case of an exception when
;; calling the hooks
(defn open-impl [tag typeid key vnode-index]
  (let [key (when key (str key))
        parent-children (or (o/get *vnode* "children") #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (o/get prev "key"))
        prev-typeid (when prev (o/get prev "typeid"))
        keymap (o/get *vnode* "keymap")]
    (o/set *vnode* "childrenCount" (inc vnode-index))
    (when (nil? (o/get *vnode* "children"))
      (o/set *vnode* "children" parent-children))
    (if (and (= typeid prev-typeid) (= key prev-key))
      (let [flag (o/get prev "keyMoved")]
        (when (or (identical? *moved-flag* flag) (identical? *new-flag* flag))
          (.error js/console
                  (str "Duplicate key: " key " in component "
                       (component-name prev))))
        (when key
          (o/set prev "keyMoved" *moved-flag*))
        (set! *vnode* prev)
        nil)
      (let [moved-vnode (and key keymap (o/get keymap key))
            flag (and moved-vnode (o/get moved-vnode "keyMoved"))]
        (if (and moved-vnode
                 (= typeid (o/get moved-vnode "typeid"))
                 (not (identical? *moved-flag* flag))
                 (not (identical? *new-flag* flag)))
          (do
            (aset parent-children vnode-index moved-vnode)
            (when (identical? moving-flag (o/get moved-vnode "keyMoved"))
              ;; the moved-node is coming from the previous children
              (o/set *vnode* "keymapInvalid" (dec (o/get *vnode* "keymapInvalid"))))
            (o/set moved-vnode "keyMoved" *moved-flag*)
            (set! *vnode* moved-vnode)
            prev)
          ;; this is a new node -> replace the node at the current index
          (let [vnode (if key
                        (new-vnode-key typeid (context/create-element tag) keymap key)
                        (new-vnode typeid (context/create-element tag)))
                flag (and moved-vnode (o/get  moved-vnode "keyMoved"))]
            (when keymap
              (o/set vnode "keyNextVnode" prev))
            ;; handle invalid states
            (cond (or (identical? *moved-flag* flag) (identical? *new-flag* flag))
                  (do
                    (.error js/console
                            (str "Duplicate key: " key " in component "
                                 (component-name moved-vnode)))
                    ;; unset the key of the already moved node, in order to avoid conflicts
                    ;; (of keys) with the newly created vnode
                    (o/set moved-vnode "key" nil))
                  (and moved-vnode (not= typeid (o/get moved-vnode "typeid")))
                  (do
                    #_(.warn
                       js/console
                       (str "Nodes with same key and different typeids. key: " key))
                    (when (identical? (o/get moved-vnode "keyMoved") moving-flag)
                      (o/set *vnode* "keymapInvalid" (dec (o/get *vnode* "keymapInvalid")))
                      ;; If the node is moving forward, it should be immediately removed because
                      ;; its key is unset
                      (set! *vnode-to-remove* moved-vnode))
                    (o/set moved-vnode "key" nil)))
            (insert-vnode-before *vnode* vnode prev vnode-index)
            (aset parent-children vnode-index vnode)
            (set! *new-node* (inc *new-node*))
            (set! *vnode* vnode)
            prev))))))

(defn open [tag typeid key will-update will-unmount will-mount get-initial-state remove-hook]
  (assert (not (nil? *component*)) (str "tag " tag " was called outside of render loop"))
  (let [prev (open-impl tag (or typeid tag) key (or (o/get *vnode* "childrenCount") 0))]
    (if (> *new-node* 0)
      (do (o/set *vnode* "componentOrCompProps" *component*)
          (when prev
            (if-let [prev-key (o/get prev "key")]
              (remove-vnode-key prev)
              (remove-vnode prev)))
          (when *vnode-to-remove*
            (remove-vnode *vnode-to-remove*)
            (set! *vnode-to-remove* nil))
          (when (= tag "foreignObject")
            (set! *svg-namespace* 0))
          (o/set *vnode* "unmount" will-unmount)
          (o/set *vnode* "removeHook" remove-hook)    
          ;; call get-initial-state and will-mount at the end to keep things consistent
          ;; in case of an exception in get-initial-state or will-mount
          (when get-initial-state
            (let [state-ref (o/get (o/get *component* "nodeOrCompData") "compDataStateRef")]
              (reset! state-ref (get-initial-state *props*))
              (set! *state* @state-ref)
              (o/set *component* "attrsOrCompState" *state*)))
          (when will-mount (will-mount *props* *state*)))
      (do
        (when prev
          (if-let [prev-key (o/get prev "key")]
            (remove-vnode-key prev)
            (remove-vnode prev)))
        (when will-update (will-update *props* *state*))
        (when (o/get *vnode* "childrenCount")
          (o/set *vnode* "childrenCount" 0))
        (clean-keymap *vnode*))))
  (set! *attrs-count* 0))

(defn close-impl [did-mount did-update]
  (clean-children *vnode*)
  (clean-keymap *vnode*)
  (reorder-nodes *vnode*)
  (if (> *new-node* 0)
    (do
      (set! *new-node* (dec *new-node*))
      (when did-mount
        (aset components-queue *components-queue-count* did-mount)
        (aset components-queue (inc *components-queue-count*) *vnode*)
        (set! *components-queue-count* (+ *components-queue-count* 2))))
    (when did-update (did-update *props* *state*))))

(defn close [did-mount did-update]
  (close-impl did-mount did-update)
  (set! *vnode* (o/get *vnode* "parentVnode")))

(defn open-comp [component-name typeid props? props comp-fn key
                 will-update will-unmount remove-hook
                 will-receive-props get-initial-state will-mount]
  (assert (not (nil? *vnode*))
          (str "tried to render " component-name " outside of render loop"))
  (let [vnode-index (or (o/get *vnode* "childrenCount") 0)
        prev (open-impl nil typeid key vnode-index)
        vnode *vnode*]
    (set! *props* (if props? props no-props-flag))
    (if (> *new-node* 0)
      (do
        (o/set *vnode* "unmount" will-unmount)
        (o/set *vnode* "removeHook" remove-hook)
        (let [state-ref (ls/->LocalStateAtom
                         nil nil on-state-change
                         #js [*vnode* comp-fn *render-queue*])]
          (o/set *vnode* "componentOrCompProps" *props*)
          (o/set *vnode* "nodeOrCompData"
                 #js {:compDataName component-name
                      :compDataStateRef state-ref
                      :compDataSvgNamespace *svg-namespace*
                      ;; index-in-parent is used when rendering a component after its local state
                      ;; has changed. We must initialize the children-count slot to the same value
                      ;; than index-in-parent
                      :compDataIndexInParent vnode-index
                      ;; the depth of the component is stored to be able to init the component-state
                      ;; var when a component is re-rendered because of a local state change
                      :compDataDepth *component-depth*
                      :compDataDirtyFlag nil
                      ;; *render-flag* is put here when the flag has been rendered.
                      ;; This must only be touched by the rendering thread !
                      :compDataRenderedFlag nil})
          ;; call will-unmount at the end to keep things consistent in case of an exception
          ;; in will-unmount
          (when prev
            (if-let [prev-key (o/get prev "key")]
              (remove-vnode-key prev)
              (remove-vnode prev)))
          (when *vnode-to-remove*
            (remove-vnode *vnode-to-remove*)
            (set! *vnode-to-remove* nil))
          ;; call get-initial-state and will-mount at the end to keep things consistent
          ;; in case of an exception in get-initial-state or will-mount
          (if get-initial-state
            (do (reset! state-ref (get-initial-state *props*))
                (set! *state* @state-ref)
                (o/set *vnode* "attrsOrCompState" *state*))
            (do (set! *state* nil)
                (o/set *vnode* "attrsOrCompState" nil)))
          (when will-mount (will-mount *props* *state*))))
      (let [prev-props (comp-props *vnode*)
            prev-state (o/get *vnode* "attrsOrCompState")
            state-ref (o/get (o/get *vnode* "nodeOrCompData") "compDataStateRef")
            state @state-ref
            comp-data (o/get *vnode* "nodeOrCompData")]
        (o/set *vnode* "componentOrCompProps" (if props? *props* no-props-flag))
        (set! *state* state)
        (o/set *vnode* "attrsOrCompState" state)
        (o/set comp-data "compDataRenderedFlag" *rendered-flag*)
        (o/set comp-data "compDataIndexInParent" vnode-index)
        (when prev
          (if-let [prev-key (o/get prev "key")]
            (remove-vnode-key prev)
            (remove-vnode prev)))
        (if (and 
             (identical? prev-props *props*)
             (identical? prev-state state)
             (not *force-render*))
          (set! *skip* true)
          (do
            (call-will-receive-props prev-props *props* state-ref will-receive-props)
            (when will-update (will-update *props* *state*))))
        (when (o/get *vnode* "childrenCount")
          (o/set *vnode* "childrenCount" 0))
        (clean-keymap *vnode*)))
    (set! *component* *vnode*)
    (set! *component-depth* (inc *component-depth*))))

(defn close-comp [parent-component did-mount did-update]
  (when-not *skip*
    (close-impl did-mount did-update))
  (set! *component* parent-component)
  (set! *component-depth* (dec *component-depth*))
  (set! *vnode* (o/get *vnode* "parentVnode"))
  (when parent-component
    (set! *props* (o/get parent-component "componentOrCompProps"))
    (set! *state* (o/get parent-component "attrsOrCompState")))
  (set! *skip* false))

;; compare-handlers-x sets this var to the previous handler in order for handle-event-handler
;; to use it
(def ^:dynamic *handlers-prev* nil)
;; compare-handlers-x sets this var to the state-ref in order for make-handler-x to use it
(def ^:dynamic *handlers-state-ref* nil)

(defn compare-handlers-static [f]
  (when (and (> *new-node* 0) (fn? f))
    (set! *handlers-state-ref* (o/get (o/get *component* "nodeOrCompData") "compDataStateRef"))
    true))

(defn compare-handlers-0 [f]
  (let [prev-attrs (or (o/get *vnode* "attrsOrCompState") #js [])
        prev-f (aget prev-attrs (inc *attrs-count*))
        state-ref (o/get (o/get *component* "nodeOrCompData") "compDataStateRef")]
    (when (nil? (o/get *vnode* "attrsOrCompState"))
      (o/set *vnode* "attrsOrCompState" prev-attrs))
    (when (not= prev-f f)
      (set! *handlers-prev* (aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-0 [handler f]
  (let [attrs (o/get *vnode* "attrsOrCompState")]
    (aset attrs *attrs-count* handler)
    (aset attrs (inc *attrs-count*) f)))

(defn compare-handlers-1 [f arg1]
  (let [prev-attrs (or (o/get *vnode* "attrsOrCompState") #js [])
        prev-f (aget prev-attrs (inc *attrs-count*))
        state-ref (o/get (o/get *component* "nodeOrCompData") "compDataStateRef")]
    (when (nil? (o/get *vnode* "attrsOrCompState"))
      (o/set *vnode* "attrsOrCompState" prev-attrs))
    (when (or (not= prev-f f)
              (not= arg1 (aget prev-attrs (+ *attrs-count* 2))))
      (set! *handlers-prev* (aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-1 [handler f arg1]
  (let [attrs (o/get *vnode* "attrsOrCompState")]
    (aset attrs *attrs-count* handler)
    (aset attrs (inc *attrs-count*) f)
    (aset attrs (+ *attrs-count* 2) arg1)))

(defn compare-handlers-2 [f arg1 arg2]
  (let [prev-attrs (or (o/get *vnode* "attrsOrCompState") #js [])
        prev-f (aget prev-attrs (inc *attrs-count*))
        state-ref (o/get (o/get *component* "nodeOrCompData") "compDataStateRef")]
    (when (nil? (o/get *vnode* "attrsOrCompState"))
      (o/set *vnode* "attrsOrCompState" prev-attrs))
    (when (or (not= prev-f f)
              (not= arg1 (aget prev-attrs (+ *attrs-count* 2)))
              (not= arg2 (aget prev-attrs (+ *attrs-count* 3))))
      (set! *handlers-prev* (aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-2 [handler f arg1 arg2]
  (let [attrs (o/get *vnode* "attrsOrCompState")]
    (aset attrs *attrs-count* handler)
    (aset attrs (inc *attrs-count*) f)
    (aset attrs (+ *attrs-count* 2) arg1)
    (aset attrs (+ *attrs-count* 3) arg2)))

(defn compare-handlers-3 [f arg1 arg2 arg3]
  (let [prev-attrs (or (o/get *vnode* "attrsOrCompState") #js [])
        prev-f (aget prev-attrs (inc *attrs-count*))
        state-ref (o/get (o/get *component* "nodeOrCompData") "compDataStateRef")]
    (when (nil? (o/get *vnode* "attrsOrCompState"))
      (o/set *vnode* "attrsOrCompState" prev-attrs))
    (when (or (not= prev-f f)
              (not= arg1 (aget prev-attrs (+ *attrs-count* 2)))
              (not= arg2 (aget prev-attrs (+ *attrs-count* 3)))
              (not= arg3 (aget prev-attrs (+ *attrs-count* 4))))
      (set! *handlers-prev* (aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-3 [handler f arg1 arg2 arg3]
  (let [attrs (o/get *vnode* "attrsOrCompState")]
    (aset attrs *attrs-count* handler)
    (aset attrs (inc *attrs-count*) f)
    (aset attrs (+ *attrs-count* 2) arg1)
    (aset attrs (+ *attrs-count* 3) arg2)
    (aset attrs (+ *attrs-count* 4) arg3)))

(defn compare-attrs [val]
  (let [prev-attrs (or (o/get *vnode* "attrsOrCompState") #js [])
        prev-val (aget prev-attrs *attrs-count*)]
    (when (nil? (o/get *vnode* "attrsOrCompState"))
      (o/set *vnode* "attrsOrCompState" prev-attrs))
    (when (not= prev-val val)
      true)))

(defn set-attr [val]
  (aset (o/get *vnode* "attrsOrCompState") *attrs-count* val))

(defn inc-attrs [count]
  (set! *attrs-count* (+ *attrs-count* count)))

(defn call-did-mount-hooks [i]
  (when (> i -1)
    (let [vnode (aget components-queue i)
          component (if (component? vnode) vnode (o/get vnode "componentOrCompProps"))
          props (comp-props component)
          state (o/get component "attrsOrCompState")]
      (set! *vnode* vnode)
      ((aget components-queue (dec i)) props state))
    (recur (- i 2))))

;; vnode is nil on first render
(defn patch-impl [render-queue parent-vnode vnode patch-fn maybe-props force-render]
  (if vnode
    (o/set parent-vnode "childrenCount"
           (o/get (o/get vnode "nodeOrCompData") "compDataIndexInParent"))
    (o/set parent-vnode "childrenCount" 0))
  (binding [*vnode* parent-vnode
            *component* nil
            *new-node* 0
            *attrs-count* 0
            *props* nil
            *state* nil
            *vnode-to-remove* nil
            *skip* false
            *force-render* force-render
            *svg-namespace* (if vnode
                              (o/get (o/get vnode "nodeOrCompData") "compDataSvgNamespace")
                              0)
            *children* nil
            *component-depth* (if vnode
                                (o/get (o/get vnode "nodeOrCompData") "compDataDepth")
                                0)
            *watch-local-state* false
            *components-queue-count* 0
            *render-queue* render-queue
            *prevent-node-removal* nil
            *handlers-prev* nil
            *handlers-state-ref* nil
            *moved-flag* #js []
            *new-flag* #js []]
    (if (identical? maybe-props no-props-flag)
      (patch-fn (when vnode (o/get vnode "key")))
      (patch-fn (when vnode (o/get vnode "key")) maybe-props))
    (set! *watch-local-state* true)
    (call-did-mount-hooks (dec *components-queue-count*))))

(defn call-post-render [post-render]
  (when post-render (post-render)))

(defn process-post-render-hooks [render-queue]
  (let [post-renders (o/get render-queue "postRenderHooks")
        global-post-render-hook (aget post-renders 0)]
    (o/set render-queue "postRenderHooks" #js [global-post-render-hook])
    (.forEach post-renders call-post-render)))

(defn process-render-queue [origin-render-queue frozen-render-queue]
  (let [frozen-dirty-comps (o/get frozen-render-queue "dirtyComps")
        l (.-length frozen-dirty-comps)
        post-render-hooks (o/get origin-render-queue "postRenderHooks")]
    ;; the vnode at depth -1 is the root vnode, not a component
    ;; Using two different depths (-1 and 0) for the root component is useful to avoid concurrent
    ;; issues (multithreads)
    (when-let [comp (aget frozen-dirty-comps 0)]
      (aset frozen-dirty-comps 0 nil)
      (let [vnode (o/get comp "vnode")
            comp-fn (o/get comp "compFn")
            props (o/get comp "props")
            post-render-fn (o/get comp "postRenderFn")]
        (when post-render-fn (.push post-render-hooks post-render-fn))
        (when vnode
          (let [vnode (aget (o/get vnode "children") 0)]
            (patch-impl origin-render-queue
                        (o/get vnode "parentVnode")
                        vnode
                        comp-fn props false)))))
    (loop [i 1]
      (when (< i l)
        (when-let [comps (aget frozen-dirty-comps i)]
          (loop []
            (when-let [comp (.pop comps)]
              (let [vnode (o/get comp "vnode")
                    comp-fn (o/get comp "compFn")
                    props (o/get comp "props")
                    post-render-fn (o/get comp "postRenderFn")]
                (when post-render-fn (.push post-render-hooks post-render-fn))
                (when vnode
                  (when (not (already-rendered-component? vnode))
                    (patch-impl origin-render-queue
                                (o/get vnode "parentVnode")
                                vnode
                                comp-fn props false))
                  (recur))))))
        (recur (inc i))))))

(defn get-render-queue [vnode]
  (cond (satisfies? vtree/VTree vnode)
        (vtree/render-queue vnode)
        (component? vnode)
        (-> ^muance.local_state.LocalStateAtom (o/get (o/get vnode "nodeOrCompData") "compDataStateRef")
            (o/get "componentData")
            (aget 2))
        :else (-> ^muance.local_state.LocalStateAtom (o/get
                                                      (o/get
                                                       (o/get vnode "componentOrCompProps")
                                                       "nodeOrCompData")
                                                      "compDataStateRef")
                  (o/get "componentData")
                  (aget 2))))

(defonce vtree-ids (atom 0))
;; Roots is only set on the rendering thread
(defonce roots #js {})

(defn get-comp-render-fn [comp]
  (-> comp
      ^muance.local_state.LocalStateAtom
      (o/get "nodeOrCompData")
      (o/get "compDataStateRef")
      (o/get "componentData")
      (aget 1)))

(defn get-user-data [k]
  (when-let [user-data (o/get *vnode* "userData")]
    (o/get user-data k)))

(defn set-user-data [k v]
  (let [user-data (o/get *vnode* "userData")]
    (if (nil? user-data)
      (let [user-data #js {}]
        (o/set user-data k v)
        (o/set *vnode* "userData" user-data))
      (o/set user-data k v))))

(defn unset-user-data [k]
  (let [user-data (o/get *vnode* "userData")]
    (when user-data
      (o/remove user-data k))))

;; node identity is the same implies that the svg-namespace value did not change

;; index-in-parent is set when moving node (including in splice) to keep things consistent
;; in case of an exception in a hook function

;; exceptions in hooks:
;; did-mount -> should prevent the call of next did-mounts, since did-mounts are called after
;; the patch process
;; will-update -> should prevent the rest of the patch process
;; did-update -> should prevent the rest of the patch process
;; will-unmount -> should prevent the removal of the node. May prevent next patch calls when
;; the exception happens on a keyed vnode, because the next patching processes will try to
;; clean the keyed node (and fail)

;; the vnode when it is created. willupdate is consistent with didupdate during a render pass.

;; synchronous rendering is mainly useful for testing. It is also useful to render in javafx tree-cells/list-cells. Synchronous rendering cannot be a parameter to patch since local state updates would not be impacted.

;; global-post-render-hook cannot be a parameter to patch since local state updates would not be impacted

;; comp-fn is a var in order for refresh-roots to work even when redefining the root component

;; list-view, tree-view ... cell factories are not used because it is hard to support with stateful vtrees / lifecycle hooks. Displaying a big list requires paging/lazy loading anyway

;; Nil properties are removed instead of the value beeing set to nil. Setting a property value
;; to nil requires using lifecycle hooks

;; Add a test for the first case of duplicate keys
;; check exceptions in will-unmount
;; get-initial-state for component children ? -- with access to the current node


