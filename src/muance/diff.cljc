(ns muance.diff
  (:refer-clojure :exclude [aget aset])
  (:require [muance.arrays :as a]
            [muance.local-state :as ls]
            [muance.objects :as o]
            [muance.vtree :as vtree]
            [muance.context :as context])
  #?(:clj (:import [java.util ArrayList HashMap])))

(def ^:const index-typeid 0)
(def ^:const index-parent-vnode 1)
(def ^:const index-node 2)
(def ^:const index-component 3)
(def ^:const index-children-count 4)
(def ^:const index-children 5)
(def ^:const index-attrs 6)
;; Unmount is stored on the node since it must be called when one of the parents of the node
;; is removed
(def ^:const index-unmount 7)
(def ^:const index-remove-hook 8)
(def ^:const index-key 9)
;; A slot which stores one of two flags:
;; - moved-flag
;; - moving-flag
;; - new-flag
;; See the documentation for these two flags for more details
(def ^:const index-key-moved 10)
;; keep track of the vnode sibling in order to reorder keyed vnodes duting child nodes
;; reconciliation
(def ^:const index-key-next-vnode 11)
(def ^:const index-keymap 12)
;; When a keyed node is removed, the keymap is marked as invalid. Invalid keymaps are
;; cleaned when the close function of the node is called
(def ^:const index-keymap-invalid 13)

(def ^:const index-text 3)

;; component specific data
(def ^:const index-comp-data 2)
(def ^:const index-comp-props 3)
(def ^:const index-comp-state 6)

(def ^:const index-comp-data-name 0)
(def ^:const index-comp-data-state-ref 1)
(def ^:const index-comp-data-svg-namespace 2)
;; index-in-parent is used when rendering a component after its local state has changed.
;; we must initialize the children-count slot to the same value than index-in-parent
(def ^:const index-comp-data-index-in-parent 3)
;; the depth of the component is stored to be able to init the component-state var when a
;; component is re-rendered because of a local state change
(def ^:const index-comp-data-depth 4)
(def ^:const index-comp-data-dirty-flag 5)

(def ^:const index-render-queue-async-fn 0)
(def ^:const index-render-queue-post-render 1)
(def ^:const index-render-queue-post-render-internal 2)
(def ^:const index-render-queue-dirty-flag 3)
(def ^:const index-render-queue-offset 4)

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
(defonce moving-flag #?(:cljs #js [] :clj (Object.)))
;; New nodes are marked as "new" during child nodes reconciliation. This is useful to be able to
;; reorder keyed nodes.
;; This flag changes on every render pass because this helps keeping things consitent, even
;; when an exception occurs
(defonce ^{:dynamic true} *new-flag* nil)
;; Set on a component "props" slot when this component does not have props. This is useful to
;; differentiate between "nil" props and no props at all. When a component does not have props,
;; no props are passed to the patch function when it is re-rendered.
(defonce no-props-flag #?(:cljs #js [] :clj (Object.)))
;; Used to enqueue components with a did-mount / will-unmount hook, and then call the hooks
;; in order
(def components-queue #?(:cljs #js [] :clj (ArrayList.)))

(def ^{:dynamic true} *state* nil)
(def ^{:dynamic true} *vnode* nil)

(declare process-render-queue)

(defn component? [vnode]
  (let [typeid (a/aget vnode index-typeid)]
    (and typeid (not #?(:cljs (string? typeid) :clj (class? typeid))) (< typeid 0))))

(defn dirty-component? [vnode]
  (a/aget vnode index-comp-data index-comp-data-dirty-flag))

(defn component-name
  "Return the fully qualified name of the node's component, as a string."
  [vnode]
  (assert vnode "muance.core/component-name expects a vnode.")
  (if (component? vnode)
    (a/aget vnode index-comp-data index-comp-data-name)
    (a/aget vnode index-component index-comp-data index-comp-data-name)))

(defn- dom-nodes* [acc vnode]
  (if (component? vnode)
    (when-let [children (a/aget vnode index-children)]
      (let [l (a/length children)]
        (loop [i 0]
          (when (< i l)
            (dom-nodes* acc (a/aget children i))
            (recur (inc i))))))
    (a/add acc (a/aget vnode index-node)))
  acc)

(declare ref-node-down)

(defn dom-nodes
  "Return a vector of all the DOM nodes associated with vnode."
  [vnode]
  (assert vnode "muance.core/dom-nodes expects a vnode.")
  (if (component? vnode)
    (into [] (dom-nodes* #?(:cljs #js [] :clj (ArrayList.)) vnode))
    [(a/aget vnode index-node)]))

(defn dom-node
  "Return the DOM nodes associated with vnode. Returns the first children of vnode if vnode is
  a component and is associated with multiple DOM nodes."
  [vnode]
  (if (component? vnode)
    (ref-node-down vnode)
    (a/aget vnode index-node)))

(defn remove-vnode-key [vnode]
  (let [parent (a/aget vnode index-parent-vnode)]
    (when-not (identical? (a/aget vnode index-key-moved) *moved-flag*)
      (a/aset vnode index-key-moved moving-flag)
      (a/aset parent index-keymap-invalid (inc (a/aget parent index-keymap-invalid))))))

(defn on-state-change [stateful-data n]
  (when *watch-local-state*
    (let [vnode (a/aget stateful-data 0)
          comp-fn (a/aget stateful-data 1)
          render-queue (a/aget stateful-data 2)
          async-fn (a/aget render-queue index-render-queue-async-fn)
          component-depth (a/aget vnode index-comp-data index-comp-data-depth)]
      (when (not (dirty-component? vnode))
        (a/aset (a/aget vnode index-comp-data) index-comp-data-dirty-flag true)
        (if-let [dirty-comps (a/aget render-queue (+ component-depth index-render-queue-offset))]
          (do (a/add dirty-comps (a/aget vnode index-comp-props))
              (a/add dirty-comps comp-fn)
              (a/add dirty-comps vnode))
          (a/aset render-queue (+ component-depth index-render-queue-offset)
                  #?(:cljs #js [(a/aget vnode index-comp-props) comp-fn vnode]
                     :clj (doto (ArrayList.)
                            (.add (a/aget vnode index-comp-props))
                            (.add comp-fn)
                            (.add vnode)))))
        (when-not (a/aget render-queue index-render-queue-dirty-flag)
          (a/aset render-queue index-render-queue-dirty-flag true)
          (if async-fn
            (async-fn (fn [] (process-render-queue render-queue)))
            (process-render-queue render-queue)))))))

(defn parent-node [parent]
  (if (component? parent)
    (recur (a/aget parent index-parent-vnode))
    (a/aget parent index-node)))

(defn remove-real-node [vnode]
  (if (component? vnode)
    (when-let [children (a/aget vnode index-children)]
      (let [l (a/length children)]
        (loop [i 0]
          (when (< i l)
            (remove-real-node (a/aget children i))
            (recur (inc i))))))
    (let [p-vnode (a/aget vnode index-parent-vnode)]
      (when p-vnode
        (context/remove-node (parent-node p-vnode) (a/aget vnode index-node))))))

(defn enqueue-unmounts [vnode]
  (when (component? vnode)
    (context/-remove-muance-watcher
     (a/aget vnode index-comp-data index-comp-data-state-ref)))
  (when (a/aget vnode index-unmount)
    (a/aset components-queue *components-queue-count* vnode)
    (set! *components-queue-count* (inc *components-queue-count*)))
  (when (and (not *prevent-node-removal*) (not *force-render*))
    (when-let [remove-hook (a/aget vnode index-remove-hook)]
      (set! *prevent-node-removal* true)
      (set! *vnode* vnode)
      (if (and (component? *vnode-to-remove*)
               (a/aget *vnode-to-remove* index-children)
               (> (a/length (a/aget *vnode-to-remove* index-children)) 1))
        (remove-hook (dom-nodes *vnode-to-remove*))
        (remove-hook (dom-node *vnode-to-remove*)))))
  (when-let [children (a/aget vnode index-children)]
    (let [children-count (a/length children)]
      (loop [i 0]
        (when (< i children-count)
          (let [child (a/aget vnode index-children i)]            
            (enqueue-unmounts child))
          (recur (inc i)))))))

(defn call-unmounts [queue-start]
  (loop [i (dec *components-queue-count*)]
    (when (>= i queue-start)
      (let [vnode (a/aget components-queue i)
            component (if (component? vnode) vnode (a/aget vnode index-component))
            props (a/aget component index-comp-props)
            state (a/aget component index-comp-state)]
        ;; *vnode* is rebound in remove-vnode
        (set! *vnode* vnode)
        ((a/aget vnode index-unmount) props state))
      (recur (dec i))))
  (set! *components-queue-count* queue-start))

(defn ref-node-down [vnode]
  (if (component? vnode)
    (when-let [children (a/aget vnode index-children)]
      (let [l (a/length children)]
        (loop [i 0]
          (when (< i l)
            (if-let [node (ref-node-down (a/aget children i))]
              node
              (recur (inc i)))))))
    (a/aget vnode index-node)))

;; find the vnode after the next child of the parent, or (recursively) of the grand-parent
;; if the parent is a component and does not have a next child with a dom node
(defn ref-node-up [vnode index-in-parent]
  ;; index-children has already been incremented
  ;; children cannot be nil
  (let [children (a/aget vnode index-children)
        l (a/length children)
        ;; if next-vnode is a keyed vnode, let's start by looking into the
        ;; next-vnode. If next-vnode is not a keyed node, it has already been removed from
        ;; the dom and thus cannot be used as a ref-node
        next-vnode (a/aget vnode index-key-next-vnode)
        found-node (loop [i index-in-parent
                          found-node (and next-vnode
                                          (not (nil? (a/aget next-vnode index-key)))
                                          (ref-node-down next-vnode))]
                     (if found-node
                       found-node
                       (when (< i l)
                         (recur (inc i) (ref-node-down (a/aget children i))))))]
    (if (nil? found-node)
      (when (component? vnode)
        (recur (a/aget vnode index-parent-vnode)
               (a/aget vnode index-parent-vnode index-children-count)))
      found-node)))

(defn insert-vnode-before* [parent-node vnode ref-node]
  (if (component? vnode)
    (when-let [children (a/aget vnode index-children)]
      (let [l (a/length children)]
        (loop [i 0]
          (when (< i l)
            (insert-vnode-before* parent-node (a/aget children i) ref-node)
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
    (let [parent-node (a/aget parent-vnode index-node)]
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
  (let [keymap-invalid (or (a/aget vnode index-keymap-invalid) 0)]
    (when (> keymap-invalid 0)
      (let [keymap (a/aget vnode index-keymap)]
        (o/forEach keymap
                   (fn #?(:cljs [v k] :clj [v k iterator])
                     (when (identical? (a/aget v index-key-moved) moving-flag)
                       (remove-vnode v)
                       #?(:cljs (o/remove keymap k)
                          :cljs (or/forEachRemove iterator))))))
      (a/aset vnode index-keymap-invalid 0))))

(defn keyed-next-vnode [vnode]
  (let [next-vnode (a/aget vnode index-key-next-vnode)]
    (if (and next-vnode (identical? moving-flag (a/aget next-vnode index-key-moved)))
      (recur next-vnode)
      next-vnode)))

(defn reorder-nodes [parent-vnode]
  (let [keymap (a/aget parent-vnode index-keymap)]
    (when keymap
      (let [children (a/aget parent-vnode index-children)]
        (loop [i (dec (a/length children))
               next-vnode nil
               next-moved? false
               next-vnode-ref nil]
          (when (> i -1)
            (let [vnode (a/aget children i)
                  prev-next-vnode (keyed-next-vnode vnode)
                  moved? (cond (not (identical? prev-next-vnode next-vnode-ref))
                               (do
                                 (insert-vnode-before parent-vnode vnode next-vnode i)
                                 true)
                               (identical? (a/aget vnode index-key-moved) *new-flag*)
                               true
                               :else false)]
              ;; text nodes are not set a next-vnode, thus they are moved more often than
              ;; needed but we don't really care
              (when (not= (a/aget vnode index-typeid) 0)
                (a/aset vnode index-key-next-vnode next-vnode))
              (recur (dec i) vnode moved? (if moved? next-vnode-ref vnode)))))))))

(defn clean-children [vnode]
  (when-let [children (a/aget vnode index-children)]
    (let [children-count (a/aget vnode index-children-count)
          children-length (a/length children)]
      (loop [l children-length]
        (when (> l children-count)
          (let [removed-vnode (a/pop children)
                k (a/aget removed-vnode index-key)]
            (if k
              (remove-vnode-key removed-vnode)
              (remove-vnode removed-vnode)))
          (recur (dec l)))))))

(defn init-keymap [keymap]
  (a/aset *vnode* index-keymap keymap)
  (a/aset *vnode* index-keymap-invalid 0)
  keymap)

(defn new-vnode [typeid element]
  #?(:cljs #js [typeid *vnode* element]
     :clj (doto (ArrayList.)
            (.add typeid)
            (.add *vnode*)
            (.add element))))

(defn new-vnode-key [typeid element keymap key]
  (let [keymap (if (nil? keymap) (init-keymap #?(:cljs #js {} :clj (HashMap.))) keymap)
        vnode #?(:cljs #js [typeid *vnode* element
                            nil nil nil nil nil nil key *new-flag*]
                 :clj (doto (ArrayList. 13)
                        (.add typeid)
                        (.add *vnode*)
                        (.add element)
                        (.add nil)
                        (.add nil)
                        (.add nil)
                        (.add nil)
                        (.add nil)
                        (.add nil)
                        (.add key)
                        (.add *new-flag*)))]
    (o/set keymap key vnode)
    vnode))

(defn call-will-receive-props [prev-props props state-ref will-receive-props]
  (when (and will-receive-props (not (identical? prev-props props)))
    (will-receive-props prev-props props state-ref)
    (set! *state* @state-ref)))

(defn comp-props [vnode]
  (let [props (a/aget vnode index-comp-props)]
    (if (identical? props no-props-flag)
      nil props)))

;; hooks are called after open-impl to keep things consistent in case of an exception when
;; calling the hooks
(defn open-impl [tag typeid key vnode-index]
  (let [key (when key (str key))
        parent-children (or (a/aget *vnode* index-children) #?(:cljs #js [] :clj (ArrayList.)))
        prev (a/aget parent-children vnode-index)
        prev-key (when prev (a/aget prev index-key))
        prev-typeid (when prev (a/aget prev index-typeid))
        keymap (a/aget *vnode* index-keymap)]
    (a/aset *vnode* index-children-count (inc vnode-index))
    (when (nil? (a/aget *vnode* index-children))
      (a/aset *vnode* index-children parent-children))
    (if (and (= typeid prev-typeid) (= key prev-key))
      (let [flag (a/aget prev index-key-moved)]
        (when (or (identical? *moved-flag* flag) (identical? *new-flag* flag))
          #?(:cljs (.error js/console
                           (str "Duplicate key: " key " in component "
                                (component-name prev)))
             :clj (binding [*out* *err*]
                    (prn (str "Duplicate key: " key " in component "
                              (component-name prev))))))
        (when key
          (a/aset prev index-key-moved *moved-flag*))
        (set! *vnode* prev)
        nil)
      (let [moved-vnode (and key keymap (o/get keymap key))
            flag (and moved-vnode (a/aget moved-vnode index-key-moved))]
        (if (and moved-vnode
                 (= typeid (a/aget moved-vnode index-typeid))
                 (not (identical? *moved-flag* flag))
                 (not (identical? *new-flag* flag)))
          (do
            (a/aset parent-children vnode-index moved-vnode)
            (when (identical? moving-flag (a/aget moved-vnode index-key-moved))
              ;; the moved-node is coming from the previous children
              (a/aset *vnode* index-keymap-invalid (dec (a/aget *vnode* index-keymap-invalid))))
            (a/aset moved-vnode index-key-moved *moved-flag*)
            (set! *vnode* moved-vnode)
            prev)
          ;; this is a new node -> replace the node at the current index
          (let [vnode (if key
                        (new-vnode-key typeid (context/create-element tag) keymap key)
                        (new-vnode typeid (context/create-element tag)))
                flag (and moved-vnode (a/aget moved-vnode index-key-moved))]
            (when keymap
              (a/aset vnode index-key-next-vnode prev))
            ;; handle invalid states
            (cond (or (identical? *moved-flag* flag) (identical? *new-flag* flag))
                  (do
                    #?(:cljs (.error js/console
                                     (str "Duplicate key: " key " in component "
                                          (component-name moved-vnode)))
                       :clj (binding [*out* *err*]
                              (prn (str "Duplicate key: " key " in component "
                                        (component-name moved-vnode)))))
                    ;; unset the key of the already moved node, in order to avoid conflicts
                    ;; (of keys) with the newly created vnode
                    (a/aset moved-vnode index-key nil))
                  (and moved-vnode (not= typeid (a/aget moved-vnode index-typeid)))
                  (do
                    #_#?(:cljs (.warn
                                js/console
                                (str "Nodes with same key and different typeids. key: " key))
                         :clj (binding [*out* *err*]
                                (prn (str "Nodes with same key and different typeids. key: "
                                          key))))
                    (when (identical? (a/aget moved-vnode index-key-moved) moving-flag)
                      (a/aset *vnode* index-keymap-invalid
                            (dec (a/aget *vnode* index-keymap-invalid)))
                      ;; If the node is moving forward, it should be immediately removed because
                      ;; its key is unset
                      (set! *vnode-to-remove* moved-vnode))
                    (a/aset moved-vnode index-key nil)))
            (insert-vnode-before *vnode* vnode prev vnode-index)
            (a/aset parent-children vnode-index vnode)
            (set! *new-node* (inc *new-node*))
            (set! *vnode* vnode)
            prev))))))

(defn open [tag typeid key will-update will-unmount remove-hook]
  (assert (not (nil? *component*)) (str "tag " tag " was called outside of render loop"))
  (let [prev (open-impl tag (or typeid tag) key
                        (or (a/aget *vnode* index-children-count) 0))]
    (if (> *new-node* 0)
      (do (a/aset *vnode* index-component *component*)
          (when prev
            (if-let [prev-key (a/aget prev index-key)]
              (remove-vnode-key prev)
              (remove-vnode prev)))
          (when *vnode-to-remove*
            (remove-vnode *vnode-to-remove*)
            (set! *vnode-to-remove* nil))
          (when (= tag "foreignObject")
            (set! *svg-namespace* 0)))
      (do
        (when prev
          (if-let [prev-key (a/aget prev index-key)]
            (remove-vnode-key prev)
            (remove-vnode prev)))
        (when will-update (will-update *props* *state*))
        (when (a/aget *vnode* index-children-count)
          (a/aset *vnode* index-children-count 0))
        (clean-keymap *vnode*))))
  (when (not= (a/aget *vnode* index-unmount) will-unmount)
    (a/aset *vnode* index-unmount will-unmount))
  (when (not= (a/aget *vnode* index-remove-hook) remove-hook)
    (a/aset *vnode* index-remove-hook remove-hook))
  (set! *attrs-count* 0))

(defn close-impl [did-mount did-update]
  (clean-children *vnode*)
  (clean-keymap *vnode*)
  (reorder-nodes *vnode*)
  (if (> *new-node* 0)
    (do
      (set! *new-node* (dec *new-node*))
      (when did-mount
        (a/aset components-queue *components-queue-count* did-mount)
        (a/aset components-queue (inc *components-queue-count*) *vnode*)
        (set! *components-queue-count* (+ *components-queue-count* 2))))
    (when did-update (did-update *props* *state*))))

(defn close [did-mount did-update]
  (close-impl did-mount did-update)
  (set! *vnode* (a/aget *vnode* index-parent-vnode)))

(def ^:const index-hooks-typeid 0)
(def ^:const index-hooks-get-initial-state 1)
(def ^:const index-hooks-will-receive-props 2)
(def ^:const index-hooks-did-mount 3)
(def ^:const index-hooks-did-update 4)
(def ^:const index-hooks-will-unmount 5)
(def ^:const index-hooks-remove 6)
(def ^:const index-hooks-will-update 7)

(defn open-comp [component-name typeid props? props comp-fn key hooks]
  (assert (not (nil? *vnode*))
          (str "tried to render " component-name " outside of render loop"))
  (let [hooks (when (and hooks (= typeid (a/aget hooks index-hooks-typeid))) hooks)
        vnode-index (or (a/aget *vnode* index-children-count) 0)
        will-unmount (when hooks (a/aget hooks index-hooks-will-unmount))
        remove-hook (when hooks (a/aget hooks index-hooks-remove))
        will-update (when hooks (a/aget hooks index-hooks-will-update))
        will-receive-props (when hooks (a/aget hooks index-hooks-will-receive-props))
        prev (open-impl nil typeid key vnode-index)
        vnode *vnode*]
    (set! *props* props)
    (when (not= (a/aget *vnode* index-unmount) will-unmount)
      (a/aset *vnode* index-unmount will-unmount))
    (when (not= (a/aget *vnode* index-remove-hook) remove-hook)
      (a/aset *vnode* index-remove-hook remove-hook))
    (if (> *new-node* 0)
      (let [state-ref #?(:cljs (ls/->LocalStateAtom
                                nil nil on-state-change
                                #js [*vnode* comp-fn *render-queue*])
                         :clj (ls/->LocalStateAtom
                               (java.util.concurrent.atomic.AtomicReference.)
                               (clojure.lang.PersistentHashMap/EMPTY)
                               on-state-change
                               (doto (ArrayList.)
                                 (.add *vnode*)
                                 (.add comp-fn)
                                 (.add *render-queue*))))
            get-initial-state (and hooks (a/aget hooks index-hooks-get-initial-state))]
        (a/aset *vnode* index-comp-props (if props? *props* no-props-flag))
        (a/aset *vnode* index-comp-data
                #?(:cljs #js[component-name state-ref *svg-namespace*
                             vnode-index *component-depth*]
                   :clj (doto (ArrayList.)
                          (.add component-name)
                          (.add state-ref)
                          (.add *svg-namespace*)
                          (.add vnode-index)
                          (.add *component-depth*))))
        ;; call will-unmount at the end to keep things consistent in case of an exception
        ;; in will-unmount
        (when prev
          (if-let [prev-key (a/aget prev index-key)]
            (remove-vnode-key prev)
            (remove-vnode prev)))
        (when *vnode-to-remove*
          (remove-vnode *vnode-to-remove*)
          (set! *vnode-to-remove* nil))
        ;; call get-initial-state at the end to keep things consistent in case of an exception
        ;; in get-initial-state
        (if get-initial-state
          (do (set! *vnode* nil)
              (reset! state-ref (get-initial-state *props*))
              (set! *vnode* vnode)
              (set! *state* @state-ref)
              (a/aset *vnode* index-comp-state *state*))
          (do (set! *state* nil)
              (a/aset *vnode* index-comp-state nil))))
      (let [prev-props (comp-props *vnode*)
            prev-state (a/aget *vnode* index-comp-state)
            state-ref (a/aget *vnode* index-comp-data index-comp-data-state-ref)
            state @state-ref
            comp-data (a/aget *vnode* index-comp-data)]
        (a/aset *vnode* index-comp-props (if props? *props* no-props-flag))
        (set! *state* state)
        (a/aset *vnode* index-comp-state state)
        (a/aset comp-data index-comp-data-dirty-flag nil)
        (a/aset comp-data index-comp-data-index-in-parent vnode-index)
        (when prev
          (if-let [prev-key (a/aget prev index-key)]
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
        (when (a/aget *vnode* index-children-count)
          (a/aset *vnode* index-children-count 0))
        (clean-keymap *vnode*)))
    (set! *component* *vnode*)
    (set! *component-depth* (inc *component-depth*))))

(defn close-comp [parent-component hooks]
  (when-not *skip*
    (if (and hooks (= (a/aget *component* index-typeid) (a/aget hooks index-hooks-typeid)))
      (close-impl (a/aget hooks index-hooks-did-mount)
                  (a/aget hooks index-hooks-did-update))
      (close-impl nil nil)))
  (set! *component* parent-component)
  (set! *component-depth* (dec *component-depth*))
  (set! *vnode* (a/aget *vnode* index-parent-vnode))
  (when parent-component
    (set! *props* (a/aget parent-component index-comp-props))
    (set! *state* (a/aget parent-component index-comp-state)))
  (set! *skip* false))

#_(defn- attr-impl [vnode ns key val set-fn]
  (let [prev-attrs (or (a/aget vnode index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-val (a/aget prev-attrs *attrs-count*)
        prev-node (a/aget vnode index-node)]
    (when (nil? (a/aget vnode index-attrs))
      (a/aset vnode index-attrs prev-attrs))
    (when (not= prev-val val)
      (a/aset prev-attrs *attrs-count* val)
      (set-fn prev-node ns key val))
    (set! *attrs-count* (inc *attrs-count*))))

#_(defn- handle-event-handlers [context on-type attrs attrs-index key handler f]
  (let [node (a/aget *vnode* index-node)
        #_handle-event-handler-fn #_(o/get context (if (identical? "listener" on-type)
                                                 "handle-listener-fn"
                                                 "handle-event-handler-fn" ))]
    (handle-event-handler context on-type node key (a/aget attrs attrs-index) handler)
    (a/aset attrs attrs-index handler)
    (a/aset attrs (inc attrs-index) f)))

;; on-type is useful to be able to register different type of event handlers (such as property
;; change listeners of javafx)
#_(defn- on-impl [context on-type key f param1 param2 param3 param-count]
  (let [prev-attrs (or (a/aget *vnode* index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-f (a/aget prev-attrs (inc *attrs-count*))
        state-ref (a/aget *component* index-comp-data index-comp-data-state-ref)]
    (when (nil? (a/aget *vnode* index-attrs))
      (a/aset *vnode* index-attrs prev-attrs))
    (cond (and (= 0 param-count) (not= prev-f f))
          (let [handler (make-handler-0 context on-type f state-ref)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f))
          (and (= 1 param-count) (or (not= prev-f f)
                                     (not= param1 (a/aget prev-attrs (+ *attrs-count* 2)))))
          (let [handler (make-handler-1 context on-type f state-ref param1)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f)
            (a/aset prev-attrs (+ *attrs-count* 2) param1))
          (and (= 2 param-count) (or (not= prev-f f)
                                     (not= param1 (a/aget prev-attrs (+ *attrs-count* 2)))
                                     (not= param2 (a/aget prev-attrs (+ *attrs-count* 3)))))
          (let [handler (make-handler-2 context on-type f state-ref param1 param2)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f)
            (a/aset prev-attrs (+ *attrs-count* 2) param1)
            (a/aset prev-attrs (+ *attrs-count* 3) param2))
          (and (= 3 param-count) (or (not= prev-f f)
                                     (not= param1 (a/aget prev-attrs (+ *attrs-count* 2)))
                                     (not= param2 (a/aget prev-attrs (+ *attrs-count* 3)))
                                     (not= param3 (a/aget prev-attrs (+ *attrs-count* 4)))))
          (let [handler (make-handler-3 context on-type f state-ref param1 param2 param3)]
            (handle-event-handlers context on-type prev-attrs *attrs-count* key handler f)
            (a/aset prev-attrs (+ *attrs-count* 2) param1)
            (a/aset prev-attrs (+ *attrs-count* 3) param2)
            (a/aset prev-attrs (+ *attrs-count* 4) param3)))
    (set! *attrs-count* (+ *attrs-count* 2 param-count))))

;; compare-handlers-x sets this var to the previous handler in order for handle-event-handler to use it
(def ^:dynamic *handlers-prev* nil)
;; compare-handlers-x sets this var to the state-ref in order for make-handler-x to use it
(def ^:dynamic *handlers-state-ref* nil)

(defn compare-handlers-static [f]
  (when (and (> *new-node* 0) (fn? f))
    (set! *handlers-state-ref*
          (a/aget *component* index-comp-data index-comp-data-state-ref))
    true))

(defn compare-handlers-0 [f]
  (let [prev-attrs (or (a/aget *vnode* index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-f (a/aget prev-attrs (inc *attrs-count*))
        state-ref (a/aget *component* index-comp-data index-comp-data-state-ref)]
    (when (nil? (a/aget *vnode* index-attrs))
      (a/aset *vnode* index-attrs prev-attrs))
    (when (not= prev-f f)
      (set! *handlers-prev* (a/aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-0 [handler f]
  (let [attrs (a/aget *vnode* index-attrs)]
    (a/aset attrs *attrs-count* handler)
    (a/aset attrs (inc *attrs-count*) f)))

(defn compare-handlers-1 [f arg1]
  (let [prev-attrs (or (a/aget *vnode* index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-f (a/aget prev-attrs (inc *attrs-count*))
        state-ref (a/aget *component* index-comp-data index-comp-data-state-ref)]
    (when (nil? (a/aget *vnode* index-attrs))
      (a/aset *vnode* index-attrs prev-attrs))
    (when (or (not= prev-f f)
              (not= arg1 (a/aget prev-attrs (+ *attrs-count* 2))))
      (set! *handlers-prev* (a/aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-1 [handler f arg1]
  (let [attrs (a/aget *vnode* index-attrs)]
    (a/aset attrs *attrs-count* handler)
    (a/aset attrs (inc *attrs-count*) f)
    (a/aset attrs (+ *attrs-count* 2) arg1)))

(defn compare-handlers-2 [f arg1 arg2]
  (let [prev-attrs (or (a/aget *vnode* index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-f (a/aget prev-attrs (inc *attrs-count*))
        state-ref (a/aget *component* index-comp-data index-comp-data-state-ref)]
    (when (nil? (a/aget *vnode* index-attrs))
      (a/aset *vnode* index-attrs prev-attrs))
    (when (or (not= prev-f f)
              (not= arg1 (a/aget prev-attrs (+ *attrs-count* 2)))
              (not= arg2 (a/aget prev-attrs (+ *attrs-count* 3))))
      (set! *handlers-prev* (a/aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-2 [handler f arg1 arg2]
  (let [attrs (a/aget *vnode* index-attrs)]
    (a/aset attrs *attrs-count* handler)
    (a/aset attrs (inc *attrs-count*) f)
    (a/aset attrs (+ *attrs-count* 2) arg1)
    (a/aset attrs (+ *attrs-count* 3) arg2)))

(defn compare-handlers-3 [f arg1 arg2 arg3]
  (let [prev-attrs (or (a/aget *vnode* index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-f (a/aget prev-attrs (inc *attrs-count*))
        state-ref (a/aget *component* index-comp-data index-comp-data-state-ref)]
    (when (nil? (a/aget *vnode* index-attrs))
      (a/aset *vnode* index-attrs prev-attrs))
    (when (or (not= prev-f f)
              (not= arg1 (a/aget prev-attrs (+ *attrs-count* 2)))
              (not= arg2 (a/aget prev-attrs (+ *attrs-count* 3)))
              (not= arg3 (a/aget prev-attrs (+ *attrs-count* 4))))
      (set! *handlers-prev* (a/aget prev-attrs *attrs-count*))
      (set! *handlers-state-ref* state-ref)
      true)))

(defn set-handler-3 [handler f arg1 arg2 arg3]
  (let [attrs (a/aget *vnode* index-attrs)]
    (a/aset attrs *attrs-count* handler)
    (a/aset attrs (inc *attrs-count*) f)
    (a/aset attrs (+ *attrs-count* 2) arg1)
    (a/aset attrs (+ *attrs-count* 3) arg2)
    (a/aset attrs (+ *attrs-count* 4) arg3)))

(defn compare-attrs [val]
  (let [prev-attrs (or (a/aget *vnode* index-attrs) #?(:cljs #js [] :clj (ArrayList.)))
        prev-val (a/aget prev-attrs *attrs-count*)]
    (when (nil? (a/aget *vnode* index-attrs))
      (a/aset *vnode* index-attrs prev-attrs))
    (when (not= prev-val val)
      true)))

(defn set-attr [val]
  (a/aset (a/aget *vnode* index-attrs) *attrs-count* val))

(defn inc-attrs [count]
  (set! *attrs-count* (+ *attrs-count* count)))

(defn call-did-mount-hooks [i]
  (when (> i -1)
    (let [vnode (a/aget components-queue i)
          component (if (component? vnode) vnode (a/aget vnode index-component))
          props (a/aget component index-comp-props)
          state (a/aget component index-comp-state)]
      (set! *vnode* vnode)
      ((a/aget components-queue (dec i)) props state))
    (recur (- i 2))))

;; vnode is nil on first render
(defn patch-impl [render-queue parent-vnode vnode patch-fn maybe-props force-render]
  (if vnode
    (a/aset parent-vnode index-children-count
            (a/aget vnode index-comp-data index-comp-data-index-in-parent))
    (a/aset parent-vnode index-children-count 0))
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
                              (a/aget vnode index-comp-data index-comp-data-svg-namespace)
                              0)
            *children* nil
            *component-depth* (if vnode
                                (a/aget vnode index-comp-data index-comp-data-depth)
                                0)
            *watch-local-state* false
            *components-queue-count* 0
            *render-queue* render-queue
            *prevent-node-removal* nil
            *handlers-prev* nil
            *handlers-state-ref* nil
            *moved-flag* #?(:cljs #js [] :clj (Object.))
            *new-flag* #?(:cljs #js [] :clj (Object.))]
    (if (identical? maybe-props no-props-flag)
      (patch-fn (when vnode (a/aget vnode index-key)))
      (patch-fn (when vnode (a/aget vnode index-key)) maybe-props))
    (set! *watch-local-state* true)
    (call-did-mount-hooks (dec *components-queue-count*))))

(defn call-post-render [post-render]
  (let [l (a/length post-render)]
    (cond
      (= l 1) ((a/aget post-render 0))
      (= l 2) ((a/aget post-render 0) (a/aget post-render 1))
      (= l 3) ((a/aget post-render 0) (a/aget post-render 1) (a/aget post-render 2))
      (= l 4) ((a/aget post-render 0) (a/aget post-render 1) (a/aget post-render 2) (a/aget post-render 3)))))

(defn call-post-render-internal [post-render]
  (post-render))

(defn process-post-render-hooks [render-queue]
  (let [post-renders (a/aget render-queue index-render-queue-post-render)]
    (a/aset render-queue index-render-queue-post-render #?(:cljs #js [] :clj (ArrayList.)))
    (a/forEach post-renders call-post-render))
  (a/forEach (a/aget render-queue index-render-queue-post-render-internal)
             call-post-render-internal))

(defn process-render-queue [render-queue]
  (a/aset render-queue index-render-queue-dirty-flag nil)
  (let [l (a/length render-queue)]
    (loop [i index-render-queue-offset]
      (when (< i l)
        (when-let [dirty-comps (a/aget render-queue i)]
          (loop []
            (let [vnode (a/pop dirty-comps)
                  comp-fn (a/pop dirty-comps)
                  props (a/pop dirty-comps)]
              ;; stop when there is no more dirty component
              (when (and vnode (dirty-component? vnode))
                (patch-impl render-queue
                            (a/aget vnode index-parent-vnode)
                            vnode
                            comp-fn props false)
                (recur)))))
        (recur (inc i)))))
  (process-post-render-hooks render-queue))

(defn patch-root-impl [vtree patch-fn props]
  ;; On first render, render synchronously
  ;; Otherwise, set the component at the top as dirty and update its props and patch-fn
  (let [vnode (vtree/vnode vtree)
        the-render-queue (vtree/render-queue vtree)
        async-fn (a/aget the-render-queue index-render-queue-async-fn)
        children (a/aget vnode index-children)]
    ;; comp is nil on first render
    (if-let [comp (a/aget children 0)]
      (do
        (if-let [dirty-comps (a/aget the-render-queue index-render-queue-offset)]
          (do
            (a/aset dirty-comps 0 props)
            (a/aset dirty-comps 1 patch-fn)
            (a/aset dirty-comps 2 comp))
          (a/aset the-render-queue index-render-queue-offset
                  #?(:cljs #js [props patch-fn comp]
                     :clj (doto (ArrayList.)
                            (.add props)
                            (.add patch-fn)
                            (.add comp)))))
        (a/aset (a/aget comp index-comp-data) index-comp-data-dirty-flag true)
        (when-not (a/aget the-render-queue index-render-queue-dirty-flag)
          (a/aset the-render-queue index-render-queue-dirty-flag true)
          (if async-fn
            (async-fn (fn [] (process-render-queue the-render-queue)))
            (process-render-queue the-render-queue))))
      (if (or (nil? async-fn) (vtree/synchronous-first-render vtree))
        (do (patch-impl the-render-queue vnode nil patch-fn props false)
            (process-post-render-hooks the-render-queue))
        (async-fn (fn []
                    (patch-impl the-render-queue vnode nil patch-fn props false)
                    (process-post-render-hooks the-render-queue)))))))

(defn get-render-queue [vnode]
  (cond (satisfies? vtree/VTree vnode)
        (vtree/render-queue vnode)
        (component? vnode)
        (-> ^muance.local_state.LocalStateAtom (a/aget vnode index-comp-data
                                                       index-comp-data-state-ref)
            (.-component-data)
            (a/aget 2))
        :else (-> ^muance.local_state.LocalStateAtom(a/aget vnode index-component
                                                            index-comp-data
                                                            index-comp-data-state-ref)
                  (.-component-data)
                  (a/aget 2))))

(defn post-render-internal
  "Post render hook for internal use only"
  [vtree f]
  (-> (get-render-queue vtree)
      (a/aget index-render-queue-post-render-internal)
      (a/add f)))

;; store hooks for components
(defonce comp-hooks #?(:cljs (js-obj) :clj (HashMap.)))

(defonce vtree-ids (atom 0))
;; Roots is only set on the rendering thread
(defonce roots #?(:cljs #js {} :clj (HashMap.)))

(defn get-comp-render-fn [comp]
  (-> comp
      ^muance.local_state.LocalStateAtom (a/aget index-comp-data index-comp-data-state-ref)
      (.-component-data)
      (a/aget 1)))

(defn refresh-root #?(:cljs [vtree id] :clj [vtree id it])
  (let [vnode (vtree/vnode vtree)
        the-render-queue (vtree/render-queue vtree)
        children (a/aget vnode index-children)]
    (when-let [comp (a/aget children 0)]
      (patch-impl the-render-queue vnode comp
                  (get-comp-render-fn comp)
                  (a/aget comp index-comp-props)
                  true)
      (process-post-render-hooks the-render-queue))))

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

;; comp-hooks stores the component-id of its target component. When redefining a component, its hooks are lost


;; setTimeout / setInterval -> store the timers with a key in an object (must be called on the render loop thread so can be mutable) in the vnode. Cancel the timer on demand (using the key) or when the component unmounts.
;; hooks-map -> Working with the Clojure :elide-meta option?
;; handlers / params - remove dynamic vars to return parameters
;; native arithmetic
