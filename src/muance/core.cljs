(ns muance.core
  (:require [goog.object :as o]))

(def ^{:private true} index-typeid 0)
(def ^{:private true} index-parent-vnode 1)
(def ^{:private true} index-node 2)
(def ^{:private true} index-children-count 3)
(def ^{:private true} index-children 4)
(def ^{:private true} index-attrs-count 5)
(def ^{:private true} index-attrs 6)
(def ^{:private true} index-state-ref 7)
(def ^{:private true} index-unmount 8)
(def ^{:private true} index-component-name 9)
(def ^{:private true} index-key 10)
(def ^{:private true} index-key-moved 11)
(def ^{:private true} index-keymap 12)
(def ^{:private true} index-keymap-invalid 13)

(def ^{:private true} index-text 3)

;; The index of a component in the vnode parent children array, plus 1.
;; The plus 1 is to avoid to have a zero value, because this var is also
;; used as a dirty flag: when the component local state changes, this var is
;; set to its opposite 
(def ^{:private true} index-comp-index-in-parent 2)
(def ^{:private true} index-comp-props 5)
(def ^{:private true} index-comp-state 6)

(def ^{:private true} vnode-stateful-key "muance.core/vnode-stateful")

(def ^{:dynamic true :private true} *current-vnode* nil)
;; Whether the current vnode has just been created or not
(def ^{:dynamic true :private true} *new-node* nil)
;; Set to the value of the moved node when a moved node is met, unless if was already set before
;; Thus it keeps the value of the higher moved node in the tree, even if child nodes are
;; themselves moved. This is necessary to know when to unset the value 
(def ^{:dynamic true :private true} *moved-vnode* nil)
;; stack didMount hooks in order to call them top down after the patching process
(def ^{:dynamic true :private true} *didMounts* nil)
(def ^{:dynamic true :private true} *props* nil)
(def ^{:dynamic true :private true} *state-ref* nil)
(def ^{:dynamic true :private true} *skip* nil)
(def ^{:dynamic true :private true} *component-depth* nil)
(def ^{:dynamic true :private true} *render-queue* nil)
;; incremented on svg open, decremented on svg close, reseted to 0 on foreignObject open,
;; previous value restored on foreignObject close
(def ^{:dynamic true :private true} *svg-namespace* nil)

(defonce ^{:private true} moved-flag nil)
(defonce ^{:private true} no-props-flag #js [])

(def svg-ns "http://www.w3.org/2000/svg")
(def xml-ns "http://www.w3.org/XML/1998/namespace")
(def xlink-ns "http://www.w3.org/1999/xlink")

(def ^:dynamic *state* nil)
(def ^:dynamic *component-name* nil)
;; *moving* = (boolean *moved-vnode*), *moving* purpose is to avoid exposing vnodes to the user,
;; since printing a vnode will cause a stackoverflow (recursive reference)
(def ^:dynamic *moving* nil)

(declare process-render-queue)

(defn- component? [vnode]
  (< (aget vnode index-typeid) 0))

(defn- dirty-component? [vnode]
  (< (aget vnode index-comp-index-in-parent) 0))

(defn- set-component-dirty [vnode dirty]
  (let [dirty-flag (aget vnode index-comp-index-in-parent)]
    (if dirty
      (when (> dirty-flag 0)
        (aset vnode index-comp-index-in-parent (- dirty-flag)))
      (when (< dirty-flag 0)
        (aset vnode index-comp-index-in-parent (- dirty-flag))))))

(defn- remove-vnode-key [vnode key]
  (aset vnode index-parent-vnode nil)
  (aset *current-vnode* index-keymap-invalid
        (inc (aget *current-vnode* index-keymap-invalid))))

(defn- on-state-change [k r o n]
  ;; we don't want to mark the component as dirty when in a render loop (willReceiveProps)
  (when (nil? *current-vnode*)
    (let [stateful-data (o/get r vnode-stateful-key)
          vnode (aget stateful-data 0)
          comp-fn (aget stateful-data 1)
          render-queue (aget stateful-data 2)
          component-depth (aget stateful-data 3)]
      (when (not (dirty-component? vnode))
        (set-component-dirty vnode true)
        (if-let [dirty-comps (aget render-queue component-depth)]
          (do (.push dirty-comps comp-fn)
              (.push dirty-comps vnode))
          (aset render-queue component-depth #js [comp-fn vnode]))
        (when-not (aget render-queue 0)
          (aset render-queue 0 true)
          (.requestAnimationFrame js/window
                                  (fn []
                                    (process-render-queue render-queue))))))))

(defn- call-unmount-hooks [vnode]
  (when-let [unmount-hook (aget vnode index-unmount)]
    ;; *component-name* is restored in remove-node
    (set! *component-name* (aget vnode index-component-name))
    (unmount-hook @(aget vnode index-state-ref))
    (when (component? vnode)
      (remove-watch (aget vnode index-state-ref) ::component))))

(defn- bottom-first-child [vnode]
  (if-let [children (aget vnode index-children)]
    (recur (aget children 0))
    vnode))

(defn- recursively-call-unmount-hooks [root]
  (let [vnode (bottom-first-child root)]
    (loop [vnode vnode]
      (when-let [children (aget vnode index-children)]
        (let [children-count (.-length children)]
          (loop [i 1]
            (when (< i children-count)
              (recursively-call-unmount-hooks (aget vnode index-children i))
              (recur (inc i))))))
      (call-unmount-hooks vnode)
      (when-not (identical? vnode root)
        (recur (aget vnode index-parent-vnode))))))

(defn- remove-real-node [vnode]
  (if (component? vnode)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (remove-real-node (aget children i))
            (recur (inc i))))))
    (let [node (aget vnode index-node)]
      (when-let [p (.-parentNode node)]
        (.removeChild p node)))))

(defn- remove-node [vnode]
  (when vnode
    (let [component-name *component-name*]
      (recursively-call-unmount-hooks vnode)
      (set! *component-name* component-name))
    (remove-real-node vnode)))

(defn- clean-keymap [vnode]
  (when-let [keymap (aget vnode index-keymap)]
    (when (> (aget vnode index-keymap-invalid) 0)
      (o/forEach keymap
                 (fn [v k]
                   (when (nil? (aget v index-parent-vnode))
                     (o/remove keymap k)
                     (remove-node v))))
      (aset vnode index-keymap-invalid 0))))

(defn- clean-children [vnode]
  (when-let [children (aget vnode index-children)]
    (let [children-count (aget vnode index-children-count)
          children-length (.-length children)]
      (loop [l children-length]
        (when (> l children-count)
          (let [removed-vnode (.pop children)
                k (aget removed-vnode index-key)]
            (if k
              (remove-vnode-key removed-vnode key)
              (remove-node removed-vnode)))
          (recur (dec l))))
      (aset vnode index-children-count 0))))

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
  (aset *current-vnode* index-keymap keymap)
  (aset *current-vnode* index-keymap-invalid 0)
  keymap)

(defn- new-vnode [typeid element]
  #js [typeid *current-vnode* element])

(defn- new-vnode-key [typeid element keymap key]
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode #js [typeid *current-vnode* element
                   nil nil nil nil nil nil nil key moved-flag]]
    (o/set keymap key vnode)
    vnode))

(defn- new-text-vnode [element text]
  #js [0 *current-vnode* element text])

(defn- create-element [tag]
  ;; tag is nil when opening a component
  (when tag
    (if (> *svg-namespace* 0)
      (.createElementNS js/document svg-ns tag)
      (.createElement js/document tag))))

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

(defn- ref-node-up [vnode]
  ;; index-children has already been incremented
  ;; children cannot be nil
  (let [children (aget vnode index-children)
        l (.-length children)
        index (aget vnode index-children-count)
        found-node (loop [i index
                          found-node nil]
                     (if found-node
                       found-node
                       (when (< i l)
                         (recur (inc i) (ref-node-down (aget children i))))))]
    (if (nil? found-node)
      (when (component? vnode)
        (recur (aget vnode index-parent-vnode)))
      found-node)))

(defn- insert-before* [parent-node vnode ref-node]
  (if (component? vnode)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (insert-before* parent-node (aget children i) ref-node)
            (recur (inc i))))))
    (.insertBefore parent-node (aget vnode index-node) ref-node)))

(defn- insert-before [parent-vnode vnode ref-vnode]
  (if (component? parent-vnode)
    (let [parent-node (parent-node parent-vnode)]
      (if-let [ref-node (when ref-vnode (ref-node-down ref-vnode))]
        (insert-before* parent-node vnode ref-node)
        (insert-before* parent-node vnode (ref-node-up parent-vnode))))
    (let [parent-node (aget parent-vnode index-node)]
      (if (nil? ref-vnode)
        (insert-before* parent-node vnode nil)
        (if-let [ref-node (ref-node-down ref-vnode)]
          (insert-before* parent-node vnode ref-node)
          (insert-before* parent-node vnode (ref-node-up parent-vnode)))))))

(defn- splice-to [nodes index moved-node to-node]
  (let [next-moved-node (aget nodes index)]
    (aset nodes index moved-node)
    (when (and next-moved-node (not (identical? next-moved-node to-node)))
      (recur nodes (inc index) next-moved-node to-node))))

(defn- will-receive-props [prev-props props willReceiveProps]
  (when (and willReceiveProps (not (identical? prev-props props)))
    (let [new-state (willReceiveProps prev-props props *state*)]
      (when-not (identical? new-state *state*)
        (reset! *state-ref* new-state)
        (set! *state* new-state)))))

(defn- comp-props [vnode]
  (let [props (aget vnode index-comp-props)]
    (if (identical? props no-props-flag)
      nil props)))

;; (= tag nil) means this is the opening of a component
(defn- open-impl [tag typeid key willUpdate willReceiveProps]
  (let [key (when key (str key))
        vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (or (aget *current-vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))
        keymap (aget *current-vnode* index-keymap)]
    (aset *current-vnode* index-children-count (inc vnode-index))
    (when (nil? (aget *current-vnode* index-children))
      (aset *current-vnode* index-children parent-children))
    (if (and (= key prev-key) (= typeid prev-typeid))
      (let [prev-state (when (nil? tag) (aget prev index-comp-state))
            prev-props (when (nil? tag) (comp-props prev))]
        (when (nil? tag)
          (set! *state-ref* (aget prev index-state-ref))
          (set! *state* @*state-ref*))
        (when key
          (aset prev index-key-moved moved-flag))
        (set! *current-vnode* prev)
        (if (and (nil? tag)
                 (identical? prev-props *props*)
                 (identical? prev-state @*state-ref*)
                 (nil? *moved-vnode*))
          (set! *skip* true)
          (do
            (when (nil? tag)
              (will-receive-props prev-props *props* willReceiveProps))
            (when willUpdate (willUpdate *props* *state*)))))
      (let [moved-vnode (and key keymap (o/get keymap key))]
        (if (and moved-vnode
                 (= typeid (aget moved-vnode index-typeid))
                 (not (identical? moved-flag (aget moved-vnode index-key-moved))))
          (let [prev-props (when (nil? tag) (comp-props moved-vnode))]
            (when (nil? *moved-vnode*)
              (set! *moved-vnode* moved-vnode)
              (set! *moving* true))
            (aset moved-vnode index-key-moved moved-flag)
            (insert-before *current-vnode* moved-vnode prev)
            (aset parent-children vnode-index moved-vnode)
            (if (aget moved-vnode index-parent-vnode)
              ;; moved-vnode is amongs the next children -> splice between the
              ;; current index and the index of the moved node
              (splice-to parent-children (inc vnode-index) prev moved-vnode)
              (do
                ;; the moved-node is coming from the previous children -> replace the node
                ;; at the current index
                (aset moved-vnode index-parent-vnode *current-vnode*)
                (aset *current-vnode* index-keymap-invalid
                      (dec (aget *current-vnode* index-keymap-invalid)))
                (if prev-key
                  (remove-vnode-key prev prev-key)
                  (remove-node prev))))
            (set! *current-vnode* moved-vnode)
            (when (nil? tag)
              (set! *state-ref* (aget moved-vnode index-state-ref))
              (set! *state* @*state-ref*)
              (will-receive-props prev-props *props* willReceiveProps))
            (when willUpdate (willUpdate *props* *state*)))
          ;; this is a new node -> replace the node at the current index
          (let [vnode (if key
                        (new-vnode-key typeid (create-element tag) keymap key)
                        (new-vnode typeid (create-element tag)))
                ;; handle invalid states
                ;; the prev-key is refreshed since it can be modified by the invalid state
                ;; handling
                prev-key (cond (and moved-vnode
                                    (identical?
                                     moved-flag (aget moved-vnode index-key-moved)))
                               (do
                                 (.error js/console
                                         (str "Duplicate key: " key
                                              " in component " *component-name*))
                                 (aset moved-vnode index-key nil)
                                 (when prev (aget prev index-key)))
                               (and moved-vnode (not= typeid (aget moved-vnode index-typeid)))
                               (do
                                 #_(.warn
                                  js/console
                                  (str "Nodes with same key and different typeids. key: " key))
                                 (aset moved-vnode index-key nil)
                                 (when (nil? (aget moved-vnode index-parent-vnode))
                                   (aset *current-vnode* index-keymap-invalid
                                         (dec (aget *current-vnode* index-keymap-invalid)))
                                   (remove-node moved-vnode))
                                 (when prev (aget prev index-key)))
                               :else prev-key)]
            (insert-before *current-vnode* vnode prev)
            (aset parent-children vnode-index vnode)
            (set! *new-node* (inc *new-node*))
            (if prev-key
              (remove-vnode-key prev prev-key)
              (remove-node prev))
            (when (= tag "foreignObject")
              (set! *svg-namespace* 0))
            (set! *current-vnode* vnode)))))
    (when (nil? tag)
      (aset *current-vnode* index-comp-index-in-parent (inc vnode-index)))))

(defn- open [tag typeid key willUpdate willUnmount]
  (assert (not (nil? *current-vnode*))
          (str "tag " tag " was called outside the render loop context"))
  (open-impl tag (or typeid tag) key willUpdate nil)
  (when (not= (aget *current-vnode* index-unmount) willUnmount)
    (aset *current-vnode* index-unmount willUnmount)
    (aset *current-vnode* index-state-ref *state-ref*)
    (aset *current-vnode* index-component-name *component-name*)))

(defn- close-impl [didMount didUpdate]
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (if (> *new-node* 0)
    (do
      (set! *new-node* (dec *new-node*))
      (when didMount
        (.push *didMounts* didMount)
        (.push *didMounts* *props*)
        (.push *didMounts* *state*)
        (.push *didMounts* *component-name*)))
    (when didUpdate (didUpdate *props* *state*)))
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil)
    (set! *moving* false)))

(defn- close [didMount didUpdate]
  (when (aget *current-vnode* index-attrs-count)
    (aset *current-vnode* index-attrs-count 0))
  (close-impl didMount didUpdate)
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn- text-node [t]
  (let [vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (or (aget *current-vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))]
    (aset *current-vnode* index-children-count (inc vnode-index))
    (when (nil? (aget *current-vnode* index-children))
      (aset *current-vnode* index-children parent-children))
    (if (= 0 prev-typeid)
      (when (not= (aget prev index-text) t)
        (aset prev index-text t)
        (o/set (aget prev index-node) "nodeValue" t))
      (let [vnode (new-text-vnode (.createTextNode js/document t) t)]
        (insert-before *current-vnode* vnode (aget parent-children vnode-index))
        (aset parent-children vnode-index vnode)
        (if prev-key
          (remove-vnode-key prev prev-key)
          (remove-node prev))))))

(def ^{:private true} hooks-key "muance.core/hooks")

(def ^{:private true} index-hooks-getInitialState 0)
(def ^{:private true} index-hooks-willReceiveProps 1)
(def ^{:private true} index-hooks-didMount 2)
(def ^{:private true} index-hooks-didUpdate 3)
(def ^{:private true} index-hooks-willUnmount 4)
(def ^{:private true} index-hooks-willUpdate 5)

(defn- open-comp [component-name typeid props? props comp-fn key hooks]
  (assert (not (nil? *current-vnode*))
          (str "tried to render " component-name " outside the render loop context"))
  (set! *component-name* component-name)
  (set! *props* props)
  (set! *component-depth* (inc *component-depth*))
  (if hooks
    (let [willUnmount (aget hooks index-hooks-willUnmount)]
      (open-impl nil typeid key
                 (aget hooks index-hooks-willUpdate)
                 (aget hooks index-hooks-willReceiveProps))
      (when (not= (aget *current-vnode* index-unmount) willUnmount)
        (aset *current-vnode* index-unmount willUnmount)
        (aset *current-vnode* index-component-name component-name)))
    (do
      (open-impl nil typeid key nil nil)
      (when (not= (aget *current-vnode* index-unmount) nil)
        (aset *current-vnode* index-unmount nil))))
  (when (> *new-node* 0)
    (if-let [getInitialState (and hooks (aget hooks index-hooks-getInitialState))]
      (set! *state-ref* (atom (getInitialState *props*)))
      (set! *state-ref* (atom nil)))
    (o/set *state-ref* vnode-stateful-key
           #js [*current-vnode* comp-fn *render-queue* *component-depth*])
    (add-watch *state-ref* ::component on-state-change)
    (set! *state* @*state-ref*))
  (aset *current-vnode* index-state-ref *state-ref*)
  (aset *current-vnode* index-comp-props (if props? *props* no-props-flag))
  (aset *current-vnode* index-comp-state *state*))

(defn- close-comp [parent-component-name parent-props parent-state-ref hooks]
  (when-not *skip*
    (if hooks
      (close-impl (aget hooks index-hooks-didMount) (aget hooks index-hooks-didUpdate))
      (close-impl nil nil)))
  (set! *component-depth* (dec *component-depth*))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode))
  (set! *props* parent-props)
  (set! *state-ref* parent-state-ref)
  (set! *state* (when parent-state-ref (cljs.core/deref parent-state-ref)))
  (set! *skip* false)
  (set! *component-name* parent-component-name))

(defn- attr-impl [ns key val set-fn]
  (let [attrs-index (or (aget *current-vnode* index-attrs-count) 0)
        prev-attrs (or (aget *current-vnode* index-attrs) #js [])
        prev-val (aget prev-attrs attrs-index)
        prev-node (aget *current-vnode* index-node)]
    (when (nil? (aget *current-vnode* index-attrs))
      (aset *current-vnode* index-attrs prev-attrs))
    (aset *current-vnode* index-attrs-count (inc attrs-index))
    (when (not= prev-val val)
      (aset prev-attrs attrs-index val)
      (set-fn prev-node ns key val))))

(defn- attr [key val]
  (attr-impl nil key (when (not (nil? val)) (str val)) set-attribute))

(defn- attr-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-attribute node nil key (str val)))))

(defn- handle-event-handlers [attrs attrs-index key handler f]
  (let [node (aget *current-vnode* index-node)]
    (when-let [prev-handler (aget attrs attrs-index)]
      (.removeEventListener node key prev-handler false))
    (when handler
      (.addEventListener node key handler false))
    (aset attrs attrs-index handler)
    (aset attrs (inc attrs-index) f)))

(defn- on-impl [key f param1 param2 param3 param-count]
  (let [attrs-index (or (aget *current-vnode* index-attrs-count) 0)
        prev-attrs (or (aget *current-vnode* index-attrs) #js [])
        prev-f (aget prev-attrs (inc attrs-index))
        state-ref *state-ref*]
    (when (nil? (aget *current-vnode* index-attrs))
      (aset *current-vnode* index-attrs prev-attrs))
    (aset *current-vnode* index-attrs-count (+ attrs-index 2 param-count))
    (cond (and (= 0 param-count) (not= prev-f f))
          (let [handler (when (fn? f) (fn [e] (f e state-ref)))]
            (handle-event-handlers prev-attrs attrs-index key handler f))
          (and (= 1 param-count) (or (not= prev-f f)
                                     (not= param1 (aget prev-attrs (+ attrs-index 2)))))
          (let [handler (when (fn? f) (fn [e] (f e state-ref param1)))]
            (handle-event-handlers prev-attrs attrs-index key handler f)
            (aset prev-attrs (+ attrs-index 2) param1))
          (and (= 2 param-count) (or (not= prev-f f)
                                     (not= param1 (aget prev-attrs (+ attrs-index 2)))
                                     (not= param2 (aget prev-attrs (+ attrs-index 3)))))
          (let [handler (when (fn? f) (fn [e] (f e state-ref param1 param2)))]
            (handle-event-handlers prev-attrs attrs-index key handler f)
            (aset prev-attrs (+ attrs-index 2) param1)
            (aset prev-attrs (+ attrs-index 3) param2))
          (and (= 3 param-count) (or (not= prev-f f)
                                     (not= param1 (aget prev-attrs (+ attrs-index 2)))
                                     (not= param2 (aget prev-attrs (+ attrs-index 3)))
                                     (not= param3 (aget prev-attrs (+ attrs-index 4)))))
          (let [handler (when (fn? f) (fn [e] (f e state-ref param1 param2 param3)))]
            (handle-event-handlers prev-attrs attrs-index key handler f)
            (aset prev-attrs (+ attrs-index 2) param1)
            (aset prev-attrs (+ attrs-index 3) param2)
            (aset prev-attrs (+ attrs-index 4) param3)))))

(defn- on [key f]
  (on-impl key f nil nil nil 0))

(defn- on-static [key f]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *current-vnode* index-node)
          state-ref *state-ref*]
      (.addEventListener node key (fn [e] (f e state-ref)) false))))

(defn- on1 [key f attr1]
  (on-impl key f attr1 nil nil 1))

(defn- on-static1 [key f attr1]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *current-vnode* index-node)
          state-ref *state-ref*]
      (.addEventListener node key (fn [e] (f e state-ref attr1)) false))))

(defn- on2 [key f attr1 attr2]
  (on-impl key f attr1 attr2 nil 2))

(defn- on-static2 [key f attr1 attr2]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *current-vnode* index-node)
          state-ref *state-ref*]
      (.addEventListener node key (fn [e] (f e state-ref attr1 attr2)) false))))

(defn- on3 [key f attr1 attr2 attr3]
  (on-impl key f attr1 attr2 attr3 3))

(defn- on-static3 [key f attr1 attr2 attr3]
  (when (and (> *new-node* 0) (fn? f))
    (let [node (aget *current-vnode* index-node)
          state-ref *state-ref*]
      (.addEventListener node key (fn [e] (f e state-ref attr1 attr2 attr3)) false))))

(defn- attr-ns [ns key val]
  (attr-impl ns key (when (not (nil? val)) (str val)) set-attribute))

(defn- attr-ns-static [ns key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-attribute node ns key (str val)))))

(defn- prop [key val]
  (attr-impl nil key val set-property))

(defn- prop-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-property node nil key val))))

(defn- input-value [val]
  (attr-impl nil "value" (when (not (nil? val)) (str val)) set-input-value))

(defn- style [key val]
  (attr-impl nil key (str val) set-style))

(defn- style-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-style node nil key (str val)))))

(defn- style-custom [key val]
  (attr-impl nil key (str val) set-style-custom))

(defn- style-custom-static [key val]
  (when (and (> *new-node* 0) (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-style-custom node nil key (str val)))))

(defn- call-did-mount-hooks [did-mount-hooks]
  (loop [l (dec (.-length did-mount-hooks))]
    (when (> l -1)
      (set! *component-name* (aget did-mount-hooks l))
      ((aget did-mount-hooks (- l 3))
       (aget did-mount-hooks (- l 2)) (aget did-mount-hooks (- l 1)))
      (recur (- l 4)))))

;; vnode is nil on first render
(defn- patch-impl [render-queue parent-vnode vnode patch-fn maybe-props]
  (set! moved-flag #js [])
  (when vnode
    (let [child-index (aget vnode index-comp-index-in-parent)]
      (aset parent-vnode index-children-count
            (if (< child-index 0) (dec (- child-index)) (dec child-index)))))
  (binding [*current-vnode* parent-vnode
            *didMounts* #js []
            *new-node* 0
            *moving* false
            *props* nil
            *state* nil
            *state-ref* nil
            *moved-vnode* nil
            *skip* false
            *svg-namespace* 0
            *component-depth* 0
            *component-name* nil
            *render-queue* render-queue]
    (if (identical? maybe-props no-props-flag)
      (patch-fn (when vnode (aget vnode index-key)))
      (patch-fn (when vnode (aget vnode index-key)) maybe-props))
    (aset parent-vnode index-children-count 0)
    (call-did-mount-hooks *didMounts*)))

(defn- process-render-queue [render-queue]
  (let [l (.-length render-queue)]
    (loop [i 1]
      (when (< i l)
        (let [dirty-comps (aget render-queue i)
              l (if dirty-comps (.-length dirty-comps) 0)]
          (loop [j (dec l)]
            (when (> j -1)
              (let [vnode (.pop dirty-comps)
                    comp-fn (.pop dirty-comps)]
                (when (dirty-component? vnode)
                  (patch-impl render-queue
                              (aget vnode index-parent-vnode) vnode
                              comp-fn (aget vnode index-comp-props))))
              (recur (- j 2)))))
        (recur (inc i)))))
  (aset render-queue 0 nil))

(defn- patch-root-impl [root patch-fn props]
  ;; On first render, render synchronously
  (if-let [children (aget root 0 index-children)]
    (let [render-queue (aget root 1)]
      (aset render-queue 0 true)
      (.requestAnimationFrame js/window
       (fn []
         (patch-impl (aget root 1)
                     (aget children 0 index-parent-vnode) (aget children 0)
                     patch-fn props)
         (process-render-queue render-queue))))
    (patch-impl (aget root 1) (aget root 0) nil patch-fn props)))

(defn- patch-root
  ([root patch-fn]
   (patch-root-impl root patch-fn no-props-flag))
  ([root patch-fn props]
   (patch-root-impl root patch-fn props)))

(defn init-vtree [node]
  (assert (and (goog/isObject node) (= 1 (.-nodeType node))) "can only patch dom nodes")
  (loop [child (.-firstChild node)]
    (when child
      (.removeChild node child)
      (recur (.-firstChild node))))
  ;; root node + render queue
  #js [#js [nil nil node 0 nil] #js []])
