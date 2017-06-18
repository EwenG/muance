(ns muance.core
  (:refer-clojure :exclude [remove key])
  (:require [goog.object :as o]))

(def ^{:private true} index-typeid 0)
(def ^{:private true} index-parent-vnode 1)
(def ^{:private true} index-node 2)
(def ^{:private true} index-component 3)
(def ^{:private true} index-children-count 4)
(def ^{:private true} index-children 5)
(def ^{:private true} index-attrs 6)
;; Unmount is stored on the node since it must be called when one of the parents of the node
;; is removed
(def ^{:private true} index-unmount 7)
(def ^{:private true} index-remove-hook 8)
(def ^{:private true} index-key 9)
;; A slot which stores one of two flags:
;; - moved-flag
;; - moving-flag
;; - new-flag
;; See the documentation for these two flags for more details
(def ^{:private true} index-key-moved 10)
;; keep track of the vnode sibling in order to reorder keyed vnodes duting child nodes
;; reconciliation
(def ^{:private true} index-key-next-vnode 11)
(def ^{:private true} index-keymap 12)
;; When a keyed node is removed, the keymap is marked as invalid. Invalid keymaps are
;; cleaned when the close function of the node is called
(def ^{:private true} index-keymap-invalid 13)

(def ^{:private true} index-text 3)

;; component specific data
(def ^{:private true} index-comp-data 2)
(def ^{:private true} index-comp-props 3)
(def ^{:private true} index-comp-state 6)

(def ^{:private true} index-comp-data-name 0)
(def ^{:private true} index-comp-data-state-ref 1)
(def ^{:private true} index-comp-data-svg-namespace 2)
;; index-in-parent is used when rendering a component after its local state has changed.
;; we must initialize the children-count slot to the same value than index-in-parent
(def ^{:private true} index-comp-data-index-in-parent 3)
;; the depth of the component is stored to be able to init the component-state var when a
;; component is re-rendered because of a local state change
(def ^{:private true} index-comp-data-depth 4)
(def ^{:private true} index-comp-data-dirty-flag 5)

(def ^{:private true} index-render-queue-async 0)
(def ^{:private true} index-render-queue-post-render 1)
(def ^{:private true} index-render-queue-dirty-flag 2)
(def ^{:private true} index-render-queue-offset 3)

;; The data needed in a local state watcher is stored on the local state itself, using this key.
(def ^{:private true} vnode-stateful-key "muance.core/vnode-stateful")

(def ^{:dynamic true :private true} *component* nil)
;; Whether the current vnode has just been created or not
(def ^{:dynamic true :private true} *new-node* nil)
(def ^{:dynamic true :private true} *attrs-count* nil)
;; Used for two different things:
;; - Used to handle a edge case when open-impl returns two vnodes to be removed.
;; See the invalid states handling in open-impl
;; - Bound to the vnode that is going to be removed when checking for remove-hooks
;; This lets the user the possibility to prevent the real DOM node removal. The user will
;; instead get a reference to the real node to remove it later
(def ^{:dynamic true :private true} *vnode-to-remove* nil)
(def ^{:dynamic true :private true} *props* nil)
;; Whether to skip a component body or not, depending on whether its props and state has changed
;; or not
(def ^{:dynamic true :private true} *skip* nil)
;; When true, components always render, even if props or state did not change
(def ^{:dynamic true :private true} *force-render* nil)
;; component-depth is used to be able to always render components top -> down. Rendering
;; components top->down avoids unecessary diffing sometimes
(def ^{:dynamic true :private true} *component-depth* nil)
;; Used to avoid re-rendering when a state update is done from a will-receive-props hook
(def ^{:dynamic true :private true} *watch-local-state* true)
(def ^{:dynamic true :private true} *components-queue-count* nil)
;; Components that need to be re-rendered are stored in the render-queue
(def ^{:dynamic true :private true} *render-queue* nil)
;; incremented on svg open, decremented on svg close, reseted to 0 on foreignObject open,
;; previous value restored on foreignObject close
(def ^{:dynamic true :private true} *svg-namespace* nil)
;; Whether a remove-hook prevents the dom node from beeing removed or not
(def ^{:dynamic true :private true} *prevent-node-removal* nil)

;; Nodes that moved because of child nodes reconciliation
;; are marked with this flag. This is useful to detect an attempt to move an already moved node,
;; a situation which can happen when duplicate keys are met.
;; This flag changes on every render pass because this helps keeping things consitent, even
;; when an exception occurs
(defonce ^{:private true} moved-flag nil)
;; Nodes are marked as "moving" during child nodes reconciliation when they get removed. We mark
;; them as moving because a removed node may be a node that is in fact moving further. So we
;; mark it as moving but keep it in the keymap so it can be added back later. If it is added
;; back later (the node moved further), then we clean the moving-flag, else (if the node really
;; is a removed node) the node is really removed when cleaning the keymap
(defonce ^{:private true} moving-flag #js [])
;; New nodes are marked as "new" during child nodes reconciliation. This is useful to be able to
;; reorder keyed nodes.
;; This flag changes on every render pass because this helps keeping things consitent, even
;; when an exception occurs
(defonce ^{:private true} new-flag nil)
;; Set on a component "props" slot when this component does not have props. This is useful to
;; differentiate between "nil" props and no props at all. When a component does not have props,
;; no props are passed to the patch function when it is re-rendered.
(defonce ^{:private true} no-props-flag #js [])
;; Used to enqueue components with a did-mount / will-unmount hook, and then call the hooks
;; in order
(def ^{:private true} components-queue #js [])

(def svg-ns "http://www.w3.org/2000/svg")
(def xml-ns "http://www.w3.org/XML/1998/namespace")
(def xlink-ns "http://www.w3.org/1999/xlink")

(def ^{:dynamic true :private true} *state* "The local state value of the current component." nil)
(def ^{:dynamic true :private true} *vnode* "The current virtual node, or component." nil)

(defn- insert-fn-dom [parent-node vnode ref-node]
  (.insertBefore parent-node (aget vnode index-node) ref-node))

(defn remove-node
  "Remove a DOM node from the DOM. Do nothing if the node has no parent."
  [node]
  (when-let [p (.-parentNode node)]
    (.removeChild p node)))

(defn- create-element [tag]
  ;; tag is nil when opening a component
  (when tag
    (if (> *svg-namespace* 0)
      (.createElementNS js/document svg-ns tag)
      (.createElement js/document tag))))

(defn- handle-event-handler [node key prev-handler handler]
  (when prev-handler
    (.removeEventListener node key prev-handler false))
  (when handler
    (.addEventListener node key handler false)))

(defn- make-handler-fn-0 [f state-ref]
  (when (fn? f) (fn [e] (f e state-ref))))

(defn- make-handler-fn-1 [f state-ref param1]
  (when (fn? f) (fn [e] (f e state-ref param1))))

(defn- make-handler-fn-2 [f state-ref param1 param2]
  (when (fn? f) (fn [e] (f e state-ref param1 param2))))

(defn- make-handler-fn-3 [f state-ref param1 param2 param3]
  (when (fn? f) (fn [e] (f e state-ref param1 param2 param3))))

(defn async-fn [f] (.requestAnimationFrame js/window f))

(def context-dom #js {:insert-fn insert-fn-dom
                      :remove-node-fn remove-node
                      :create-element-fn create-element
                      :handle-event-handler-fn handle-event-handler
                      :make-handler-0 make-handler-fn-0
                      :make-handler-1 make-handler-fn-1
                      :make-handler-2 make-handler-fn-2
                      :make-handler-3 make-handler-fn-3
                      :async-fn async-fn})

(defn state []
  (assert (not (nil? *vnode*)) (str "muance.core/state was called outside a render loop"))
  *state*)

(defn vnode []
  (assert (not (nil? *vnode*)) (str "muance.core/vnode was called outside a render loop"))
  *vnode*)

(declare process-render-queue)

(defn- component? [vnode]
  (< (aget vnode index-typeid) 0))

(defn- dirty-component? [vnode]
  (aget vnode index-comp-data index-comp-data-dirty-flag))

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  [vnode]
  (assert vnode "muance.core/component-name expects a vnode.")
  (if (component? vnode)
    (aget vnode index-comp-data index-comp-data-name)
    (aget vnode index-component index-comp-data index-comp-data-name)))

(defn- dom-nodes* [acc vnode]
  (if (component? vnode)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (dom-nodes* acc (aget children i))
            (recur (inc i))))))
    (.push acc (aget vnode index-node)))
  acc)

(declare ref-node-down)

(defn dom-nodes
  "Return an javascript array of all the DOM nodes associated with vnode."
  [vnode]
  (assert vnode "muance.core/dom-nodes expects a vnode.")
  (if (component? vnode)
    (dom-nodes* #js [] vnode)
    #js [(aget vnode index-node)]))

(defn dom-node
  "Return the DOM nodes associated with vnode. Returns the first children of vnode if vnode is
  a component and is associated with multiple DOM nodes."
  [vnode]
  (assert vnode "muance.core/dom-node expects a vnode.")
  (if (component? vnode)
    (ref-node-down vnode)
    (aget vnode index-node)))

(defn key
  "Returns the :muance.core/key attribute of vnode, as a string."
  [vnode]
  (assert vnode "muance.core/key expects a vnode.")
  (aget vnode index-key))

(defn set-timeout
  "Execute f after a delay expressed in milliseconds. The first argument of f is the local state reference of the vnode component."
  [vnode f millis]
  (assert vnode "muance.core/set-timeout expects a vnode.")
  (let [component (if (component? vnode) vnode (aget vnode index-component))
        state-ref (aget component index-comp-data index-comp-data-state-ref)]
    (.setTimeout js/window (fn [] (f state-ref)) millis)))

(defn set-interval
  "Periodically execute f. The period is expressed in milliseconds. The first argument of f is the local state reference of the vnode component."
  [vnode f millis]
  (assert vnode "muance.core/set-timeout expects a vnode.")
  (let [component (if (component? vnode) vnode (aget vnode index-component))
        state-ref (aget component index-comp-data index-comp-data-state-ref)]
    (.setInterval js/window (fn [] (f state-ref)) millis)))

(defn- remove-vnode-key [vnode key]
  (let [parent (aget vnode index-parent-vnode)]
    (when-not (identical? (aget vnode index-key-moved) moved-flag)
      (aset vnode index-key-moved moving-flag)
      (aset parent index-keymap-invalid (inc (aget parent index-keymap-invalid))))))

(defn- on-state-change [k r o n]
  (when *watch-local-state*
    (let [stateful-data (o/get r vnode-stateful-key)
          vnode (aget stateful-data 0)
          comp-fn (aget stateful-data 1)
          render-queue (aget stateful-data 2)
          async-fn (aget stateful-data 3)
          async (aget render-queue index-render-queue-async)
          component-depth (aget vnode index-comp-data index-comp-data-depth)]
      (when (not (dirty-component? vnode))
        (aset (aget vnode index-comp-data) index-comp-data-dirty-flag true)
        (if-let [dirty-comps (aget render-queue (+ component-depth index-render-queue-offset))]
          (do (.push dirty-comps (aget vnode index-comp-props))
              (.push dirty-comps comp-fn)
              (.push dirty-comps vnode))
          (aset render-queue (+ component-depth index-render-queue-offset)
                #js [(aget vnode index-comp-props) comp-fn vnode]))
        (when-not (aget render-queue index-render-queue-dirty-flag)
          (aset render-queue index-render-queue-dirty-flag true)
          (if async
            (async-fn (fn [] (process-render-queue render-queue)))
            (process-render-queue render-queue)))))))

(defn- remove-real-node [context vnode]
  (if (component? vnode)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (remove-real-node context (aget children i))
            (recur (inc i))))))
    (let [node (aget vnode index-node)
          remove-node-fn (o/get context "remove-node-fn")]
      (remove-node-fn node))))

(defn- enqueue-unmounts [vnode]
  (when (component? vnode)
    (remove-watch (aget vnode index-comp-data index-comp-data-state-ref) ::component))
  (when (aget vnode index-unmount)
      (aset components-queue *components-queue-count* vnode)
      (set! *components-queue-count* (inc *components-queue-count*)))
  (when (and (not *prevent-node-removal*) (not *force-render*))
    (when-let [remove-hook (aget vnode index-remove-hook)]
      (set! *prevent-node-removal* true)
      (set! *vnode* vnode)
      (if (and (component? *vnode-to-remove*)
               (aget *vnode-to-remove* index-children)
               (> (.-length (aget *vnode-to-remove* index-children)) 1))
        (remove-hook (dom-nodes *vnode-to-remove*))
        (remove-hook (dom-node *vnode-to-remove*)))))
  (when-let [children (aget vnode index-children)]
    (let [children-count (.-length children)]
      (loop [i 0]
        (when (< i children-count)
          (let [child (aget vnode index-children i)]            
            (enqueue-unmounts child))
          (recur (inc i)))))))

(defn- call-unmounts [queue-start]
  (loop [i (dec *components-queue-count*)]
    (when (>= i queue-start)
      (let [vnode (aget components-queue i)
            component (if (component? vnode) vnode (aget vnode index-component))
            props (aget component index-comp-props)
            state (aget component index-comp-state)]
        ;; *vnode* is rebound in remove-vnode
        (set! *vnode* vnode)
        ((aget vnode index-unmount) props state))
      (recur (dec i))))
  (set! *components-queue-count* queue-start))

(defn- parent-node [parent]
  (if (component? parent)
    (recur (aget parent index-parent-vnode))
    (aget parent index-node)))

(defn- ref-node-down [vnode]
  (if (component? vnode)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (if-let [node (ref-node-down (aget children i))]
              node
              (recur (inc i)))))))
    (aget vnode index-node)))

;; find the vnode after the next child of the parent, or (recursively) of the grand-parent
;; if the parent is a component and does not have a next child with a dom node
(defn- ref-node-up [vnode index-in-parent]
  ;; index-children has already been incremented
  ;; children cannot be nil
  (let [children (aget vnode index-children)
        l (.-length children)
        ;; if next-vnode is a keyed vnode, let's start by looking into the
        ;; next-vnode. If next-vnode is not a keyed node, it has already been removed from
        ;; the dom and thus cannot be used as a ref-node
        next-vnode (aget vnode index-key-next-vnode)
        found-node (loop [i index-in-parent
                          found-node (and next-vnode
                                          (not (nil? (aget next-vnode index-key)))
                                          (ref-node-down next-vnode))]
                     (if found-node
                       found-node
                       (when (< i l)
                         (recur (inc i) (ref-node-down (aget children i))))))]
    (if (nil? found-node)
      (when (component? vnode)
        (recur (aget vnode index-parent-vnode)
               (aget vnode index-parent-vnode index-children-count)))
      found-node)))

(defn- insert-vnode-before* [context parent-node vnode ref-node]
  (if (component? vnode)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (insert-vnode-before* context parent-node (aget children i) ref-node)
            (recur (inc i))))))
    (let [insert-fn (o/get context "insert-fn")]
      (insert-fn parent-node vnode ref-node))))

;; index-in-parent is passed as a parameter because, when reordering keyed nodes, the
;; index-children-count slot has already been incremented by all children
(defn- insert-vnode-before [context parent-vnode vnode ref-vnode index-in-parent]
  (if (component? parent-vnode)
    (let [parent-node (parent-node parent-vnode)]
      (if-let [ref-node (when ref-vnode (ref-node-down ref-vnode))]
        (insert-vnode-before*
         context parent-node vnode ref-node)
        (insert-vnode-before*
         context parent-node vnode (ref-node-up parent-vnode (inc index-in-parent)))))
    (let [parent-node (aget parent-vnode index-node)]
      (if (nil? ref-vnode)
        (insert-vnode-before* context parent-node vnode nil)
        (if-let [ref-node (ref-node-down ref-vnode)]
          (insert-vnode-before*
           context parent-node vnode ref-node)
          (insert-vnode-before*
           context parent-node vnode (ref-node-up parent-vnode (inc index-in-parent))))))))

(defn- remove-vnode [context vnode]
  (let [current-vnode *vnode*
        queue-start *components-queue-count*]
    (binding [*vnode-to-remove* vnode
              *prevent-node-removal* false]
      (enqueue-unmounts vnode)
      (call-unmounts queue-start)
      (set! *vnode* current-vnode)
      (when (not *prevent-node-removal*)
        (remove-real-node context vnode)))))

(defn- clean-keymap [context vnode]
  (let [keymap-invalid (or (aget vnode index-keymap-invalid) 0)]
    (when (> keymap-invalid 0)
      (let [keymap (aget vnode index-keymap)]
        (o/forEach keymap
                   (fn [v k]
                     (when (identical? (aget v index-key-moved) moving-flag)
                       (remove-vnode context v)
                       (o/remove keymap k)))))
      (aset vnode index-keymap-invalid 0))))

(defn- keyed-next-vnode [vnode]
  (let [next-vnode (aget vnode index-key-next-vnode)]
    (if (and next-vnode (identical? moving-flag (aget next-vnode index-key-moved)))
      (recur next-vnode)
      next-vnode)))

(defn- reorder-nodes [context parent-vnode]
  (let [keymap (aget parent-vnode index-keymap)]
    (when keymap
      (let [children (aget parent-vnode index-children)]
        (loop [i (dec (.-length children))
               next-vnode nil
               next-moved? false
               next-vnode-ref nil]
          (when (> i -1)
            (let [vnode (aget children i)
                  prev-next-vnode (keyed-next-vnode vnode)
                  moved? (cond (not (identical? prev-next-vnode next-vnode-ref))
                               (do
                                 (insert-vnode-before context parent-vnode vnode next-vnode i)
                                 true)
                               (identical? (aget vnode index-key-moved) new-flag)
                               true
                               :else false)]
              ;; text nodes are not set a next-vnode, thus they are moved more often than
              ;; needed but we don't really care
              (when (not= (aget vnode index-typeid) 0)
                (aset vnode index-key-next-vnode next-vnode))
              (recur (dec i) vnode moved? (if moved? next-vnode-ref vnode)))))))))

(defn- clean-children [context vnode]
  (when-let [children (aget vnode index-children)]
    (let [children-count (aget vnode index-children-count)
          children-length (.-length children)]
      (loop [l children-length]
        (when (> l children-count)
          (let [removed-vnode (.pop children)
                k (aget removed-vnode index-key)]
            (if k
              (remove-vnode-key removed-vnode key)
              (remove-vnode context removed-vnode)))
          (recur (dec l)))))))

(defn- set-attribute [node ns key val]
  (if (nil? val)
    (.removeAttribute node key)
    (if (nil? ns)
      (.setAttribute node key val)
      (.setAttributeNS node ns key val))))

(defn- set-property [node ns key val]
  (o/set node key val))

(defn- set-input-value [node ns key val]
  (when (not= (o/get node key) val)
    (o/set node key val)))

(defn- set-style [node ns key val]
  (o/set (.-style node) key val))

(defn- set-style-custom [node ns key val]
  (.setProperty (.-style node) key val))

(defn- init-keymap [keymap]
  (aset *vnode* index-keymap keymap)
  (aset *vnode* index-keymap-invalid 0)
  keymap)

(defn- new-vnode [typeid element]
  #js [typeid *vnode* element])

(defn- new-vnode-key [typeid element keymap key]
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode #js [typeid *vnode* element
                   nil nil nil nil nil nil key new-flag]]
    (o/set keymap key vnode)
    vnode))

(defn- new-text-vnode [element text]
  #js [0 *vnode* element text])

(defn- call-will-receive-props [prev-props props state-ref will-receive-props]
  (when (and will-receive-props (not (identical? prev-props props)))
    (will-receive-props prev-props props state-ref)
    (set! *state* @state-ref)))

(defn- comp-props [vnode]
  (let [props (aget vnode index-comp-props)]
    (if (identical? props no-props-flag)
      nil props)))

;; hooks are called after open-impl to keep things consistent in case of an exception when
;; calling the hooks
(defn- open-impl [context tag typeid key vnode-index]
  (let [key (when key (str key))
        parent-children (or (aget *vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))
        keymap (aget *vnode* index-keymap)]
    (aset *vnode* index-children-count (inc vnode-index))
    (when (nil? (aget *vnode* index-children))
      (aset *vnode* index-children parent-children))
    (if (and (= typeid prev-typeid) (= key prev-key))
      (do (when key
            (aset prev index-key-moved moved-flag))
          (set! *vnode* prev)
          nil)
      (let [moved-vnode (and key keymap (o/get keymap key))
            flag (and moved-vnode (aget moved-vnode index-key-moved))]
        (if (and moved-vnode
                 (= typeid (aget moved-vnode index-typeid))
                 (not (identical? moved-flag flag))
                 (not (identical? new-flag flag)))
          (do
            (aset parent-children vnode-index moved-vnode)
            (when (identical? moving-flag (aget moved-vnode index-key-moved))
              ;; the moved-node is coming from the previous children
              (aset *vnode* index-keymap-invalid (dec (aget *vnode* index-keymap-invalid))))
            (aset moved-vnode index-key-moved moved-flag)
            (set! *vnode* moved-vnode)
            prev)
          ;; this is a new node -> replace the node at the current index
          (let [create-element-fn (o/get context "create-element-fn")
                vnode (if key
                        (new-vnode-key typeid (create-element-fn tag) keymap key)
                        (new-vnode typeid (create-element-fn tag)))
                flag (and moved-vnode (aget moved-vnode index-key-moved))]
            (when keymap
              (aset vnode index-key-next-vnode prev))
            ;; handle invalid states
            (cond (or (identical? moved-flag flag) (identical? new-flag flag))
                  (do
                    (.error js/console
                            (str "Duplicate key: " key " in component "
                                 (component-name moved-vnode)))
                    ;; unset the key of the already moved node, in order to avoid conflicts
                    ;; (of keys) with the newly created vnode
                    (aset moved-vnode index-key nil))
                  (and moved-vnode (not= typeid (aget moved-vnode index-typeid)))
                  (do
                    #_(.warn
                       js/console
                       (str "Nodes with same key and different typeids. key: " key))
                    (when (identical? (aget moved-vnode index-key-moved) moving-flag)
                      (aset *vnode* index-keymap-invalid
                            (dec (aget *vnode* index-keymap-invalid)))
                      ;; If the node is moving forward, it should be immediately removed because
                      ;; its key is unset
                      (set! *vnode-to-remove* moved-vnode))
                    (aset moved-vnode index-key nil)))
            (insert-vnode-before context *vnode* vnode prev vnode-index)
            (aset parent-children vnode-index vnode)
            (set! *new-node* (inc *new-node*))
            (set! *vnode* vnode)
            prev))))))

(defn- open-contextualized [context tag typeid key will-update will-unmount remove-hook]
  (assert (not (nil? *component*)) (str "tag " tag " was called outside a render loop"))
  (let [prev (open-impl context tag (or typeid tag) key
                        (or (aget *vnode* index-children-count) 0))]
    (if (> *new-node* 0)
      (do (aset *vnode* index-component *component*)
          (when prev
            (if-let [prev-key (aget prev index-key)]
              (remove-vnode-key prev prev-key)
              (remove-vnode context prev)))
          (when *vnode-to-remove*
            (remove-vnode context *vnode-to-remove*)
            (set! *vnode-to-remove* nil))
          (when (= tag "foreignObject")
            (set! *svg-namespace* 0)))
      (do
        (when prev
          (if-let [prev-key (aget prev index-key)]
            (remove-vnode-key prev prev-key)
            (remove-vnode context prev)))
        (when will-update (will-update *props* *state*))
        (when (aget *vnode* index-children-count)
          (aset *vnode* index-children-count 0))
        (clean-keymap context *vnode*))))
  (when (not= (aget *vnode* index-unmount) will-unmount)
    (aset *vnode* index-unmount will-unmount))
  (when (not= (aget *vnode* index-remove-hook) remove-hook)
    (aset *vnode* index-remove-hook remove-hook))
  (set! *attrs-count* 0))

(defn- open [tag typeid key will-update will-unmount remove-hook]
  (open-contextualized context-dom tag typeid key will-update will-unmount remove-hook))

(defn- close-impl [context did-mount did-update]
  (clean-children context *vnode*)
  (clean-keymap context *vnode*)
  (reorder-nodes context *vnode*)
  (if (> *new-node* 0)
    (do
      (set! *new-node* (dec *new-node*))
      (when did-mount
        (aset components-queue *components-queue-count* did-mount)
        (aset components-queue (inc *components-queue-count*) *vnode*)
        (set! *components-queue-count* (+ *components-queue-count* 2))))
    (when did-update (did-update *props* *state*))))

(defn- close-contextualized [context did-mount did-update]
  (close-impl context did-mount did-update)
  (set! *vnode* (aget *vnode* index-parent-vnode)))

(defn- close [did-mount did-update]
  (close-contextualized context-dom did-mount did-update))

(defn- text-node [t]
  (let [vnode-index (or (aget *vnode* index-children-count) 0)
        parent-children (or (aget *vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))]
    (aset *vnode* index-children-count (inc vnode-index))
    (when (nil? (aget *vnode* index-children))
      (aset *vnode* index-children parent-children))
    (if (= 0 prev-typeid)
      (when (not= (aget prev index-text) t)
        (aset prev index-text t)
        (o/set (aget prev index-node) "nodeValue" t))
      (let [vnode (new-text-vnode (.createTextNode js/document t) t)]
        (insert-vnode-before
         context-dom *vnode* vnode (aget parent-children vnode-index) vnode-index)
        (aset parent-children vnode-index vnode)
        (if prev-key
          (remove-vnode-key prev prev-key)
          (when prev (remove-vnode context-dom prev)))))))

(def ^{:private true} index-hooks-get-initial-state 0)
(def ^{:private true} index-hooks-will-receive-props 1)
(def ^{:private true} index-hooks-did-mount 2)
(def ^{:private true} index-hooks-did-update 3)
(def ^{:private true} index-hooks-will-unmount 4)
(def ^{:private true} index-hooks-remove 5)
(def ^{:private true} index-hooks-will-update 6)

(defn- open-comp-contextualized [context component-name typeid props? props comp-fn key hooks]
  (assert (not (nil? *vnode*))
          (str "tried to render " component-name " outside a render loop"))
  (let [vnode-index (or (aget *vnode* index-children-count) 0)
        will-unmount (when hooks (aget hooks index-hooks-will-unmount))
        remove-hook (when hooks (aget hooks index-hooks-remove))
        will-update (when hooks (aget hooks index-hooks-will-update))
        will-receive-props (when hooks (aget hooks index-hooks-will-receive-props))
        prev (open-impl context nil typeid key vnode-index)
        vnode *vnode*]
    (set! *props* props)
    (when (not= (aget *vnode* index-unmount) will-unmount)
      (aset *vnode* index-unmount will-unmount))
    (when (not= (aget *vnode* remove-hook) remove-hook)
      (aset *vnode* index-remove-hook remove-hook))
    (if (> *new-node* 0)
      (let [state-ref (atom nil)
            get-initial-state (and hooks (aget hooks index-hooks-get-initial-state))]
        (o/set state-ref vnode-stateful-key
               #js [*vnode* comp-fn *render-queue* (o/get context "async-fn")])
        (add-watch state-ref ::component on-state-change)
        (aset *vnode* index-comp-props (if props? *props* no-props-flag))
        (aset *vnode* index-comp-data
              #js[component-name state-ref *svg-namespace* vnode-index *component-depth*])
        ;; call will-unmount at the end to keep things consistent in case of an exception
        ;; in will-unmount
        (when prev
          (if-let [prev-key (aget prev index-key)]
            (remove-vnode-key prev prev-key)
            (remove-vnode context prev)))
        (when *vnode-to-remove*
          (remove-vnode context *vnode-to-remove*)
          (set! *vnode-to-remove* nil))
        ;; call get-initial-state at the end to keep things consistent in case of an exception
        ;; in get-initial-state
        (if get-initial-state
          (do (set! *vnode* nil)
              (reset! state-ref (get-initial-state *props*))
              (set! *vnode* vnode)
              (set! *state* @state-ref)
              (aset *vnode* index-comp-state *state*))
          (do (set! *state* nil)
              (aset *vnode* index-comp-state nil))))
      (let [prev-props (comp-props *vnode*)
            prev-state (aget *vnode* index-comp-state)
            state-ref (aget *vnode* index-comp-data index-comp-data-state-ref)
            state @state-ref
            comp-data (aget *vnode* index-comp-data)]
        (aset *vnode* index-comp-props (if props? *props* no-props-flag))
        (set! *state* state)
        (aset *vnode* index-comp-state state)
        (aset comp-data index-comp-data-dirty-flag nil)
        (aset comp-data index-comp-data-index-in-parent vnode-index)
        (when prev
          (if-let [prev-key (aget prev index-key)]
            (remove-vnode-key prev prev-key)
            (remove-vnode context prev)))
        (if (and 
             (identical? prev-props *props*)
             (identical? prev-state state)
             (not *force-render*))
          (set! *skip* true)
          (do
            (call-will-receive-props prev-props *props* state-ref will-receive-props)
            (when will-update (will-update *props* *state*))))
        (when (aget *vnode* index-children-count)
          (aset *vnode* index-children-count 0))
        (clean-keymap context *vnode*)))
    (set! *component* *vnode*)
    (set! *component-depth* (inc *component-depth*))))

(defn- open-comp [component-name typeid props? props comp-fn key hooks]
  (open-comp-contextualized context-dom component-name typeid props? props comp-fn key hooks))

(defn- close-comp-contextualized [context parent-component hooks]
  (when-not *skip*
    (if hooks
      (close-impl context
                  (aget hooks index-hooks-did-mount)
                  (aget hooks index-hooks-did-update))
      (close-impl context nil nil)))
  (set! *component* parent-component)
  (set! *component-depth* (dec *component-depth*))
  (set! *vnode* (aget *vnode* index-parent-vnode))
  (when parent-component
    (set! *props* (aget parent-component index-comp-props))
    (set! *state* (aget parent-component index-comp-state)))
  (set! *skip* false))

(defn- close-comp [parent-component hooks]
  (close-comp-contextualized context-dom parent-component hooks))

(defn- attr-impl [vnode ns key val set-fn]
  (let [prev-attrs (or (aget vnode index-attrs) #js [])
        prev-val (aget prev-attrs *attrs-count*)
        prev-node (aget vnode index-node)]
    (when (nil? (aget vnode index-attrs))
      (aset vnode index-attrs prev-attrs))
    (when (not= prev-val val)
      (aset prev-attrs *attrs-count* val)
      (set-fn prev-node ns key val))
    (set! *attrs-count* (inc *attrs-count*))))

(defn- handle-event-handlers [context on-type attrs attrs-index key handler f]
  (let [node (aget *vnode* index-node)
        handle-event-handler-fn (o/get context (if (identical? "listener" on-type)
                                                 "handle-listener-fn"
                                                 "handle-event-handler-fn" ))]
    (handle-event-handler-fn node key (aget attrs attrs-index) handler)
    (aset attrs attrs-index handler)
    (aset attrs (inc attrs-index) f)))

;; on-type is useful to be able to register different type of event handlers (such as property
;; change listeners of javafx)
(defn- on-impl [context on-type key f param1 param2 param3 param-count]
  (let [prev-attrs (or (aget *vnode* index-attrs) #js [])
        prev-f (aget prev-attrs (inc *attrs-count*))
        state-ref (aget *component* index-comp-data index-comp-data-state-ref)
        make-handler-0 (o/get context (if (identical? "listener" on-type)
                                        "make-listener-0" "make-handler-0" ))
        make-handler-1 (o/get context (if (identical? "listener" on-type)
                                        "make-listener-1" "make-handler-1"))
        make-handler-2 (o/get context (if (identical? "listener" on-type)
                                        "make-listener-2" "make-handler-2"))
        make-handler-3 (o/get context (if (identical? "listener" on-type)
                                        "make-listener-3" "make-handler-3"))]
    (when (nil? (aget *vnode* index-attrs))
      (aset *vnode* index-attrs prev-attrs))
    (cond (and (= 0 param-count) (not= prev-f f))
          (let [handler (make-handler-0 f state-ref)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f))
          (and (= 1 param-count) (or (not= prev-f f)
                                     (not= param1 (aget prev-attrs (+ *attrs-count* 2)))))
          (let [handler (make-handler-1 f state-ref param1)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f)
            (aset prev-attrs (+ *attrs-count* 2) param1))
          (and (= 2 param-count) (or (not= prev-f f)
                                     (not= param1 (aget prev-attrs (+ *attrs-count* 2)))
                                     (not= param2 (aget prev-attrs (+ *attrs-count* 3)))))
          (let [handler (make-handler-2 f state-ref param1 param2)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f)
            (aset prev-attrs (+ *attrs-count* 2) param1)
            (aset prev-attrs (+ *attrs-count* 3) param2))
          (and (= 3 param-count) (or (not= prev-f f)
                                     (not= param1 (aget prev-attrs (+ *attrs-count* 2)))
                                     (not= param2 (aget prev-attrs (+ *attrs-count* 3)))
                                     (not= param3 (aget prev-attrs (+ *attrs-count* 4)))))
          (let [handler (make-handler-3 f state-ref param1 param2 param3)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f)
            (aset prev-attrs (+ *attrs-count* 2) param1)
            (aset prev-attrs (+ *attrs-count* 3) param2)
            (aset prev-attrs (+ *attrs-count* 4) param3)))
    (set! *attrs-count* (+ *attrs-count* 2 param-count))))

(defn- on [key f]
  (on-impl context-dom "handler" key f nil nil nil 0))

(defn- on-static [key f]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *vnode* index-node)
          state-ref (aget *component* index-comp-data index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-0 f state-ref)))))

(defn- on1 [key f attr1]
  (on-impl context-dom "handler" key f attr1 nil nil 1))

(defn- on-static1 [key f attr1]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *vnode* index-node)
          state-ref (aget *component* index-comp-data index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-1 f state-ref attr1)))))

(defn- on2 [key f attr1 attr2]
  (on-impl context-dom "handler" key f attr1 attr2 nil 2))

(defn- on-static2 [key f attr1 attr2]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *vnode* index-node)
          state-ref (aget *component* index-comp-data index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-2 f state-ref attr1 attr2)))))

(defn- on3 [key f attr1 attr2 attr3]
  (on-impl context-dom "handler" key f attr1 attr2 attr3 3))

(defn- on-static3 [key f attr1 attr2 attr3]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *vnode* index-node)
          state-ref (aget *component* index-comp-data index-comp-data-state-ref)]
      (handle-event-handler node key nil (make-handler-fn-3 f state-ref attr1 attr2 attr3)))))

(defn- attr-ns [ns key val]
  (attr-impl *vnode* ns key (when (not (nil? val)) (str val)) set-attribute))

(defn- attr-ns-static [ns key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *vnode* index-node)]
      (set-attribute node ns key (str val)))))

(defn- prop [key val]
  (if (> *svg-namespace* 0)
    (attr-impl *vnode* nil key (when (not (nil? val)) (str val)) set-attribute)
    (attr-impl *vnode* nil key val set-property)))

(defn- prop-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *vnode* index-node)]
      (if (> *svg-namespace* 0)
        (set-attribute node nil key (str val))
        (set-property node nil key val)))))

(defn- input-value [val]
  (attr-impl *vnode* nil "value" (when (not (nil? val)) (str val)) set-input-value))

(defn- style [key val]
  (attr-impl *vnode* nil key (str val) set-style))

(defn- style-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *vnode* index-node)]
      (set-style node nil key (str val)))))

(defn- style-custom [key val]
  (attr-impl *vnode* nil key (str val) set-style-custom))

(defn- style-custom-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *vnode* index-node)]
      (set-style-custom node nil key (str val)))))

(defn- call-did-mount-hooks [i]
  (when (> i -1)
    (let [vnode (aget components-queue i)
          component (if (component? vnode) vnode (aget vnode index-component))
          props (aget component index-comp-props)
          state (aget component index-comp-state)]
      (set! *vnode* vnode)
      ((aget components-queue (dec i)) props state))
    (recur (- i 2))))

;; vnode is nil on first render
(defn- patch-impl [render-queue parent-vnode vnode patch-fn maybe-props force-render]
  (set! moved-flag #js [])
  (set! new-flag #js [])
  (if vnode
    (aset parent-vnode index-children-count
          (aget vnode index-comp-data index-comp-data-index-in-parent))
    (aset parent-vnode index-children-count 0))
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
                              (aget vnode index-comp-data index-comp-data-svg-namespace)
                              0)
            *component-depth* (if vnode
                                (aget vnode index-comp-data index-comp-data-depth)
                                0)
            *watch-local-state* false
            *components-queue-count* 0
            *render-queue* render-queue
            *prevent-node-removal* nil]
    (if (identical? maybe-props no-props-flag)
      (patch-fn (when vnode (aget vnode index-key)))
      (patch-fn (when vnode (aget vnode index-key)) maybe-props))
    (set! *watch-local-state* true)
    (call-did-mount-hooks (dec *components-queue-count*))))

(defn- call-post-render [post-render]
  (case (.-length post-render)
    1 ((aget post-render 0))
    2 ((aget post-render 0) (aget post-render 1))
    3 ((aget post-render 0) (aget post-render 1) (aget post-render 2))
    4 ((aget post-render 0) (aget post-render 1) (aget post-render 2) (aget post-render 3))))

(defn- process-post-render-hooks [render-queue]
  (let [post-renders (aget render-queue index-render-queue-post-render)]
    (loop [i 0]
      (let [post-render (aget post-renders i)]
        (when-not (nil? post-render)
          (call-post-render post-render)
          (recur (inc i)))))
    (aset render-queue index-render-queue-post-render #js [])))

(defn- process-render-queue [render-queue]
  (aset render-queue index-render-queue-dirty-flag nil)
  (let [l (.-length render-queue)]
    (loop [i index-render-queue-offset]
      (when (< i l)
        (when-let [dirty-comps (aget render-queue i)]
          (loop []
            (let [vnode (.pop dirty-comps)
                  comp-fn (.pop dirty-comps)
                  props (.pop dirty-comps)]
              ;; stop when there is no more dirty component
              (when (and vnode (dirty-component? vnode))
                (patch-impl render-queue
                            (aget vnode index-parent-vnode) vnode
                            comp-fn props false)
                (recur)))))
        (recur (inc i)))))
  (process-post-render-hooks render-queue))

(defn- patch-root-impl [vtree patch-fn props]
  ;; On first render, render synchronously
  ;; Otherwise, set the component at the top as dirty and update its props and patch-fn
  (let [vnode (.-vnode vtree)
        render-queue (.-render-queue vtree)
        async (aget render-queue index-render-queue-async)
        children (aget vnode index-children)]
        ;; comp is nil on first render
    (if-let [comp (aget children 0)]
      (do
        (if-let [dirty-comps (aget render-queue index-render-queue-offset)]
          (do
            (aset dirty-comps 0 props)
            (aset dirty-comps 1 patch-fn)
            (aset dirty-comps 2 comp))
          (aset render-queue index-render-queue-offset #js [props patch-fn comp]))
        (aset (aget comp index-comp-data) index-comp-data-dirty-flag true)
        (when-not (aget render-queue index-render-queue-dirty-flag)
          (aset render-queue index-render-queue-dirty-flag true)
          (if async
            (async-fn (fn [] (process-render-queue render-queue)))
            (process-render-queue render-queue))))
      (do
        (patch-impl render-queue vnode nil patch-fn props false)
        (process-post-render-hooks render-queue)))))

;; id is used to keep track of rendered vtrees, for re-rendering on function/component reload
(deftype VTree [vnode render-queue id])

(defn- get-render-queue [vnode]
  (cond (instance? VTree vnode)
        (.-render-queue vnode)
        (component? vnode)
        (-> (aget vnode index-comp-data index-comp-data-state-ref)
            (o/get vnode-stateful-key)
            (aget 2))
        :else (-> (aget vnode index-component index-comp-data
                        index-comp-data-state-ref)
                  (o/get vnode-stateful-key)
                  (aget 2))))

(defn post-render
  "Registers a function to be executed after the next Muance render pass. Takes a vnode or vtree,
  the function to be executed and up to three optional parameters to be passed to the 
  function f."
  ([vnode f]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (get-render-queue vnode)
       (aget index-render-queue-post-render)
       (.push #js [f])))
  ([vnode f arg1]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (get-render-queue vnode)
       (aget index-render-queue-post-render)
       (.push #js [f arg1])))
  ([vnode f arg1 arg2]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (get-render-queue vnode)
       (aget index-render-queue-post-render)
       (.push #js [f arg1 arg2])))
  ([vnode f arg1 arg2 arg3]
   (assert vnode "muance.core/post-render expects a vnode.")
   (-> (get-render-queue vnode)
       (aget index-render-queue-post-render)
       (.push #js [f arg1 arg2 arg3]))))

;; store hooks for components
(defonce ^{:private true} comp-hooks (js-obj))

(defonce ^{:private true} vtree-ids (volatile! 0))
(defonce ^{:private true} roots #js {})

(defn- new-root-vnode []
  #js [nil nil (.createDocumentFragment js/document) nil 0 #js []])

(defn- get-comp-render-fn [comp]
  (-> comp
      (aget index-comp-data index-comp-data-state-ref)
      (o/get vnode-stateful-key)
      (aget 1)))

(defn- refresh-root [vtree id roots]
  (let [vnode (.-vnode vtree)
        render-queue (.-render-queue vtree)
        children (aget vnode index-children)]
    (when-let [comp (aget children 0)]
      (patch-impl render-queue vnode comp
                  (get-comp-render-fn comp)
                  (aget comp index-comp-props)
                  true))))

(defn- refresh-roots []
  (o/forEach roots refresh-root))

(defn vtree
  "Creates a new vtree. By default the vtree is rendered asynchronously. When async is false,
  the vtree is rendered synchronously."
  ([]
   (vtree true))
  ([async]
   (VTree. (new-root-vnode)
           ;; async + post render hooks
           #js [async #js []]
           (vswap! vtree-ids inc))))

(defn patch
  "Patch a vtree using component. The optional third argument is the component props."
  ([vtree component]
   (patch-root-impl vtree component no-props-flag))
  ([vtree component props]
   (patch-root-impl vtree component props)))

(defn remove [vtree]
  "Remove a vtree from the DOM. A removed vtree can still be patched and added back to the DOM."
  (let [vnode (.-vnode vtree)
        fragment (.createDocumentFragment js/document)]
    (when-let [comp (aget vnode index-children 0)]
      (insert-vnode-before* context-dom fragment comp nil))
    (aset vnode index-node fragment)
    (o/remove roots (.-id vtree))))

(defn insert-before [vtree ref-node]
  "Inserts the DOM node(s) associated with vtree in the DOM, before ref-node."
  (let [parent-node (.-parentNode ref-node)
        vnode (.-vnode vtree)]
    (when-let [comp (aget vnode index-children 0)]
      (insert-vnode-before* context-dom parent-node comp ref-node))
    (aset vnode index-node parent-node)
    (o/set roots (.-id vtree) vtree)))

(defn append-child [vtree parent-node]
  "Inserts the DOM node(s) associated with vtree in the DOM, as the last child(ren) of 
parent-node."
  (let [vnode (.-vnode vtree)]
    (when-let [comp (aget vnode index-children 0)]
      (insert-vnode-before* context-dom parent-node comp nil))
    (aset vnode index-node parent-node)
    (o/set roots (.-id vtree) vtree)))

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
;; clean the keyed node (and fail).
