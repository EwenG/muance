(ns muance.core
  (:refer-clojure :exclude [class])
  (:require [goog.dom :as dom]
            [goog.object :as o]))

(def ^{:private true} index-typeid 0)
(def ^{:private true} index-parent-vnode 1)
(def ^{:private true} index-node 2)
(def ^{:private true} index-children-count 3)
(def ^{:private true} index-children 4)
(def ^{:private true} index-attrs-count 5)
(def ^{:private true} index-attrs 6)
(def ^{:private true} index-unmounts 7)
(def ^{:private true} index-styles-count 8)
(def ^{:private true} index-styles 9)
(def ^{:private true} index-key 10)
(def ^{:private true} index-keymap 11)
(def ^{:private true} index-keymap-invalid 12)

(def ^{:private true} node-data-key "muance.core/node-data")
(def ^{:private true} node-state-ref-key "muance.core/node-state-ref")

(def ^{:dynamic true :private true} *current-vnode* nil)
(def ^{:dynamic true :private true} *key* nil)
;; Incremented on every call to open when the patching process is in a subtree of a newly
;; created node. Decremented on every call to close in the same subtree 
(def ^{:dynamic true :private true} *new-node* nil)
;; true after a call to open, false after a call to close
(def ^{:dynamic true :private true} *in-attrs-position* nil)
;; stack didMount hooks in order to call them top down after the patching process
(def ^{:dynamic true :private true} *didMounts* nil)
(def ^{:dynamic true :private true} *props* nil)
(def ^{:dynamic true :private true} *state-ref* nil)

(def ^:dynamic *state* nil)
;; Set to the value of the moved node when a moved node is met, unless if was already set before
;; Thus it keeps the value of the higher moved node in the tree, even if child nodes are
;; themselves moved. This is necessary to know when to unset the value 
(def ^:dynamic *did-move* nil)

(defn- remove-vnode-key [vnode key]
  (aset vnode index-parent-vnode nil)
  (aset *current-vnode* index-keymap-invalid true))

(defn- call-unmount-hooks [vnode]
  (when-let [unmount-hooks (aget vnode index-unmounts)]
    (loop [l (dec (.-length unmount-hooks))]
      (when (> l -1)
        ((aget unmount-hooks l) (o/get vnode node-state-ref-key))
        (recur (dec l))))))

(defn- bottom-first-child [vnode]
  (if-let [children (aget vnode index-children)]
    (recur (aget children 0))
    vnode))

(defn- recursively-call-unmount-hooks [root]
  (let [vnode (bottom-first-child root)]
    (loop [vnode vnode]
      (when-not (identical? vnode root)
        (call-unmount-hooks vnode)
        (when-let [children (aget vnode index-children)]
          (let [children-count (.-length children)]
            (loop [i 1]
              (when (< i children-count)
                (recursively-call-unmount-hooks (aget vnode index-children i))
                (recur (inc i))))))
        (recur (aget vnode index-parent-vnode)))))
  (call-unmount-hooks root))

(defn- remove-node [vnode]
  (when vnode
    (recursively-call-unmount-hooks vnode)
    (dom/removeNode (aget vnode index-node))))

(defn- clean-keymap [vnode]
  (when-let [keymap (aget vnode index-keymap)]
    (when (aget vnode index-keymap-invalid)
      (o/forEach keymap
                 (fn [v k]
                   (when-not (aget v index-parent-vnode)
                     (o/remove v k)
                     (remove-node v))))
      (aset vnode index-keymap-invalid false))))

(defn- clean-children [vnode]
  (when-let [children (aget vnode index-children)]
    (let [children-count (aget vnode index-children-count)
          children-length (.-length children)]
      (loop [l children-length]
        (when (not= l children-count)
          (let [removed-vnode (.pop children)
                k (aget removed-vnode index-key)]
            (if k
              (remove-vnode-key removed-vnode key)
              (remove-node removed-vnode)))))
      (aset vnode index-children-count 0))))

(defn- new-element [vnode index tag]
  (let [new-node (.createElement js/document tag)
        ref-vnode (aget vnode index-children (inc index))]
    (.insertBefore (aget vnode index-node) new-node
                   (when ref-vnode (aget ref-vnode index-node)))
    new-node))

(defn- noop [vnode props state])

(defn- init-keymap [keymap]
  (o/set *current-vnode* index-keymap keymap)
  keymap)

(defn- new-vnode [typeid element willUnmount]
  #js [typeid *current-vnode* element])

(defn- new-vnode-unmount [typeid element willUnmount]
  #js [typeid *current-vnode* element 0 nil 0 nil #js [willUnmount]])

(defn- new-vnode-key [typeid element willUnmount keymap key]
  (assert (or (nil? keymap) (not (o/containsKey keymap key)))
          (str "Duplicate key: " key) )
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode #js [typeid *current-vnode* element 0 nil 0 nil nil 0 nil key]]
    (o/set keymap key vnode)
    vnode))

(defn- new-vnode-key-unmount [typeid element willUnmount keymap key]
  (assert (or (nil? keymap) (not (o/containsKey keymap key)))
          (str "Duplicate key: " key) )
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode #js [typeid *current-vnode* element 0 nil 0 nil #js [willUnmount] 0 nil key]]
    (o/set keymap key vnode)
    vnode))

(defn- splice-to [nodes index moved-node to-node]
  (assert (not (nil? moved-node)) (str "Duplicate key: " (aget to-node index-key)) )
  (let [next-moved-node (aget nodes index)]
    (aset nodes index moved-node)
    (when (and next-moved-node (not (identical? next-moved-node to-node)))
      (recur nodes (inc index) next-moved-node to-node))))

(defn- open-lifecycle-impl [tag willUpdate willUnmount new-vnode set-state-ref?]
  (let [vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (or (aget *current-vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))]
    (set! *in-attrs-position* true)
    (aset *current-vnode* index-children-count (inc vnode-index))
    (when-not (aget *current-vnode* index-children)
      (aset *current-vnode* index-children parent-children))
    (if (= tag prev-typeid)
      (do (set! *current-vnode* prev)
          (willUpdate prev *props* *state*))
      (let [element (new-element *current-vnode* vnode-index tag)
            vnode (new-vnode tag element willUnmount)]
        (aset parent-children vnode-index vnode)
        (when set-state-ref? (o/set vnode node-state-ref-key *state-ref*))
        (set! *new-node* (inc *new-node*))
        (if prev-key
          (remove-vnode-key prev prev-key)
          (remove-node prev))
        (set! *current-vnode* vnode)))
    *current-vnode*))

(defn- open-typeid-lifecycle-impl [tag typeid willUpdate willUnmount
                                   new-vnode new-vnode-key set-state-ref?]
  (let [key (when *key* (str *key*))
        vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (or (aget *current-vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))
        keymap (aget *current-vnode* index-keymap)
        moved-vnode (and key keymap (o/get keymap key))]
    (set! *key* nil)
    (set! *in-attrs-position* true)
    (aset *current-vnode* index-children-count (inc vnode-index))
    (when-not (aget *current-vnode* index-children)
      (aset *current-vnode* index-children parent-children))
    (if (or (and key (= key prev-key))
            (and (nil? key) (nil? prev-key) (= typeid prev-typeid)))
      ;; same key or no keys and same typeid -> nothing to do
      (do
        ;; TODO assert tags are equal when same key 
        (when key nil)
        (set! *current-vnode* prev)
        (willUpdate prev *props* *state*))
      (if moved-vnode
        (do (when (nil? *did-move*)
              (set! *did-move* moved-vnode))
            (willUpdate prev *props* *state*)
            (dom/insertSiblingBefore (aget moved-vnode index-node)
                                     (aget prev index-node))
            (aset parent-children vnode-index moved-vnode)
            (if (aget moved-vnode index-parent-vnode)
              ;; moved-vnode and the moved-vnode is amongs the next children -> splice
              ;; between the current index and the index of the moved node
              (splice-to parent-children (inc vnode-index) prev moved-vnode)
              (do
                ;; the moved-node is coming from the previous children -> replace the node
                ;; at the current index
                (aset moved-vnode index-parent-vnode *current-vnode*)
                (if prev-key
                  (remove-vnode-key prev prev-key)
                  (remove-node prev))))
            (set! *current-vnode* moved-vnode))
        ;; this is a new node -> replace the node at the current index
        (let [element (new-element *current-vnode* vnode-index tag)
              vnode (if key
                      (new-vnode-key typeid element willUnmount keymap key)
                      (new-vnode typeid element willUnmount))]
          (aset parent-children vnode-index vnode)
          (when set-state-ref? (o/set vnode node-state-ref-key *state-ref*))
          (set! *new-node* (inc *new-node*))
          (if prev-key
            (remove-vnode-key prev prev-key)
            (remove-node prev))
          (set! *current-vnode* vnode))))
    *current-vnode*))

(defn open [tag]
  (open-lifecycle-impl tag noop noop new-vnode false))

(defn open-lifecycle [tag willUpdate willUnmount]
  (if willUnmount
    (open-lifecycle-impl tag (or willUpdate noop) willUnmount new-vnode-unmount true)
    (open-lifecycle-impl tag (or willUpdate noop) noop new-vnode false)))

(defn open-typeid [tag typeid]
  (open-typeid-lifecycle-impl tag typeid noop noop new-vnode new-vnode-key false))

(defn open-typeid-lifecycle [tag typeid willUpdate willUnmount]
  (if willUnmount
    (open-typeid-lifecycle-impl tag typeid (or willUpdate noop) willUnmount
                                new-vnode-unmount new-vnode-key-unmount true)
    (open-typeid-lifecycle-impl tag typeid (or willUpdate noop) noop
                                new-vnode new-vnode-key false)))

(defn close []
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (set! *in-attrs-position* false)
  (when (> *new-node* 0)
    (set! *new-node* (dec *new-node*)))
  (when (identical? *did-move* *current-vnode*)
    (set! *did-move* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn close-lifecycle [didMount didUpdate]
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (set! *in-attrs-position* false)
  (if (> *new-node* 0)
    (do (set! *new-node* (dec *new-node*))
        (.push *didMounts* didMount)
        (.push *didMounts* *props*)
        (.push *didMounts* *state*))
    (didUpdate *props* *state*))
  (when (identical? *did-move* *current-vnode*)
    (set! *did-move* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn attr [key val])
(defn attr-static [key val])
(defn class1 [c1])
(defn class2 [c1 c2])
(defn class3 [c1 c2 c3])
(defn class [classes])
(defn class-static1 [c1])
(defn class-static2 [c1 c2])
(defn class-static3 [c1 c2 c3])
(defn class-static [& classes])
(defn style [key val])
(defn style-static [key val])

(defn- call-did-mount-hooks [did-mount-hooks]
  (loop [l (dec (.-length did-mount-hooks))]
    (when (> l -1)
      ((aget did-mount-hooks (- l 2)) (aget did-mount-hooks (- l 1)) (aget did-mount-hooks l))
      (recur (- l 3)))))

(defn- patch-impl [node patch-fn props? props]
  (assert (dom/isNodeLike node "can only patch dom nodes"))
  (if-let [node-data (o/get node node-data-key)]
    (binding [*current-vnode* node-data
              *key* nil
              *didMounts* #js []
              *new-node* 0
              *in-attrs-position* false
              *props* nil
              *state* nil
              *state-ref* nil
              *did-move* nil]
      (if props?
        (patch-fn props)
        (patch-fn))
      (clean-children *current-vnode*)
      (clean-keymap *current-vnode*)
      (call-did-mount-hooks *didMounts*))
    (binding [*current-vnode* #js [nil nil node 0 #js []]
              *key* nil
              *didMounts* #js []
              *new-node* 0
              *in-attrs-position* false
              *props* nil
              *state* nil
              *state-ref* nil
              *did-move* nil]
      (o/set node node-data-key *current-vnode*)
      (dom/removeChildren node)
      (if props?
        (patch-fn props)
        (patch-fn))
      (clean-children *current-vnode*)
      (clean-keymap *current-vnode*)
      (call-did-mount-hooks *didMounts*))))

(defn patch
  ([node patch-fn] (patch-impl node patch-fn false nil))
  ([node patch-fn props] (patch-impl node patch-fn true props)))
