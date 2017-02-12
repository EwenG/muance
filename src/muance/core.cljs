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

(def ^{:private true} index-text 3)

(def ^{:private true} node-data-key "muance.core/node-data")
(def ^{:private true} node-state-ref-key "muance.core/node-state-ref")

(def ^{:dynamic true :private true} *current-vnode* nil)
(def ^{:dynamic true :private true} *key* nil)
;; Incremented on every call to open when the patching process is in a subtree of a newly
;; created node. Decremented on every call to close in the same subtree 
(def ^{:dynamic true :private true} *new-node* nil)
;; Set to the value of the moved node when a moved node is met, unless if was already set before
;; Thus it keeps the value of the higher moved node in the tree, even if child nodes are
;; themselves moved. This is necessary to know when to unset the value 
(def ^{:dynamic true :private true} *moved-vnode* nil)
;; true after a call to open, false after a call to close
(def ^{:dynamic true :private true} *in-attrs-position* nil)
;; stack didMount hooks in order to call them top down after the patching process
(def ^{:dynamic true :private true} *didMounts* nil)
(def ^{:dynamic true :private true} *props* nil)
(def ^{:dynamic true :private true} *state-ref* nil)

(def ^:dynamic *state* nil)

;; debugging pupose only
(defn print-vnode [vnode]
  (loop [i 0]
    (when (<= i index-keymap-invalid)
      (when (not= i index-parent-vnode)
        (pr (aget vnode i)) (print " "))
      (recur (inc i))))
  (prn))

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
      (when-let [children (aget vnode index-children)]
        (let [children-count (.-length children)]
          (loop [i 1]
            (when (< i children-count)
              (recursively-call-unmount-hooks (aget vnode index-children i))
              (recur (inc i))))))
      (call-unmount-hooks vnode)
      (when-not (identical? vnode root)
        (recur (aget vnode index-parent-vnode))))))

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
        (when (> l children-count)
          (let [removed-vnode (.pop children)
                k (aget removed-vnode index-key)]
            (if k
              (remove-vnode-key removed-vnode key)
              (remove-node removed-vnode)))
          (recur (dec l))))
      (aset vnode index-children-count 0))))

(defn- set-attribute [node key val]
  (cond (nil? val) (.removeAttribute node key)
        (fn? val) (o/set node key val)
        :else (.setAttribute node key val)))

(defn- remove-attribute [node val key]
  (if (fn? val)
    (o/set node key nil)
    (.removeAttribute node key)))

(defn- set-style [node key val]
  (o/set (.-style node) key val))

(defn- remove-style [node val key]
  (o/set (.-style node) key ""))

(defn- clean-attrs-impl [vnode index-count index remove-fn]
  (when-let [attrs (aget vnode index)]
    (let [attrs-count (aget vnode index-count)
          attrs-length (.-length attrs)]
      (loop [l attrs-length]
        (when (> l attrs-count)
          (remove-fn (aget vnode index-node) (.pop attrs) (.pop attrs))
          (recur (- l 2))))
      (aset vnode index-count 0))))

(defn- clean-attrs [vnode]
  (clean-attrs-impl vnode index-attrs-count index-attrs remove-attribute))

(defn- clean-styles [vnode]
  (clean-attrs-impl vnode index-styles-count index-styles remove-style))

(defn- new-element [vnode index tag]
  (let [new-node (.createElement js/document tag)
        ref-vnode (aget vnode index-children (inc index))]
    (.insertBefore (aget vnode index-node) new-node
                   (when ref-vnode (aget ref-vnode index-node)))
    new-node))

(defn- new-text-element [vnode index text]
  (let [new-node (.createTextNode js/document text)
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

(defn- new-text-vnode [element text]
  #js [-1 *current-vnode* element text])

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
        (set! *current-vnode* vnode)))))

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
        (when (and key (= key prev-key))
          (assert
           (= tag prev-typeid)
           (str "Nodes with same key but different tags or typeid. Keys: " key " " prev-key)))
        (set! *current-vnode* prev)
        (willUpdate *props* *state*))
      (if moved-vnode
        (do (when (nil? *moved-vnode*)
              (set! *moved-vnode* moved-vnode))
            (willUpdate *props* *state*)
            (.insertBefore (aget *current-vnode* index-node)
                           (aget moved-vnode index-node)
                           (when prev (aget prev index-node)))
            (aset parent-children vnode-index moved-vnode)
            (if (aget moved-vnode index-parent-vnode)
              ;; moved-vnode is amongs the next children -> splice between the
              ;; current index and the index of the moved node
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
          (set! *current-vnode* vnode))))))

(defn- open [tag]
  (open-lifecycle-impl tag noop noop new-vnode false))

(defn- open-lifecycle [tag willUpdate willUnmount]
  (if willUnmount
    (open-lifecycle-impl tag (or willUpdate noop) willUnmount new-vnode-unmount true)
    (open-lifecycle-impl tag (or willUpdate noop) noop new-vnode false)))

(defn- open-typeid [tag typeid]
  (open-typeid-lifecycle-impl tag typeid noop noop new-vnode new-vnode-key false))

(defn- open-typeid-lifecycle [tag typeid willUpdate willUnmount]
  (if willUnmount
    (open-typeid-lifecycle-impl tag typeid (or willUpdate noop) willUnmount
                                new-vnode-unmount new-vnode-key-unmount true)
    (open-typeid-lifecycle-impl tag typeid (or willUpdate noop) noop
                                new-vnode new-vnode-key false)))

(defn- text-node [t]
  (let [vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (or (aget *current-vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))]
    (aset *current-vnode* index-children-count (inc vnode-index))
    (when-not (aget *current-vnode* index-children)
      (aset *current-vnode* index-children parent-children))
    (if (= -1 prev-typeid)
      (when (not= (aget prev index-text) t)
        (o/set (aget prev index-node) "nodeValue" t))
      (let [element (new-text-element *current-vnode* vnode-index t)
            vnode (new-text-vnode element t)]
        (aset parent-children vnode-index vnode)
        (if prev-key
          (remove-vnode-key prev prev-key)
          (remove-node prev))))))

(defn- close []
  (clean-attrs *current-vnode*)
  (clean-styles *current-vnode*)
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (set! *in-attrs-position* false)
  (when (> *new-node* 0)
    (set! *new-node* (dec *new-node*)))
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn- close-lifecycle [didMount didUpdate]
  (clean-attrs *current-vnode*)
  (clean-styles *current-vnode*)
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (set! *in-attrs-position* false)
  (if (> *new-node* 0)
    (when didMount
      (set! *new-node* (dec *new-node*))
      (.push *didMounts* didMount)
      (.push *didMounts* *props*)
      (.push *didMounts* *state*))
    (when didUpdate (didUpdate *props* *state*)))
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn- attr-impl [key val index-count index set-fn remove-fn]
  (let [attrs-index (or (aget *current-vnode* index-count) 0)
        prev-attrs (or (aget *current-vnode* index) #js [])
        prev-node (aget *current-vnode* index-node)]
    (when-not (aget *current-vnode* index)
      (aset *current-vnode* index prev-attrs))
    (loop [i 0]
      (let [k (aget prev-attrs i)]
        (if (< i attrs-index)
          (if (= key k)
            (let [v (aget prev-attrs (inc i))]
              (when (not= v val)
                (aset prev-attrs (inc i) val)
                (set-fn prev-node key val)))
            (recur (+ i 2)))
          (do
            (aset *current-vnode* index-count (+ attrs-index 2))
            (cond (nil? k)
                  (do (aset prev-attrs i key)
                      (aset prev-attrs (inc i) val)
                      (set-fn prev-node key val))
                  (= key k)
                  (let [v (aget prev-attrs (inc i))]
                    (when (not= v val)
                      (aset prev-attrs (inc i) val)
                      (set-fn prev-node key val)))
                  :else
                  ;; (not= key k)
                  (let [v (aget prev-attrs (inc i))]
                    (remove-fn prev-node v k)
                    (aset prev-attrs i key)
                    (aset prev-attrs (inc i) val)
                    (set-fn prev-node key val)))))))))

(defn- attr [key val]
  (attr-impl key (when (not (nil? val)) (str val))
             index-attrs-count index-attrs set-attribute remove-attribute))

(defn- attr-static [key val]
  (when (and *new-node* (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-attribute node key (str val)))))

(defn- class1 [c1]
  (attr "class" c1))

(defn- class2 [c1 c2]
  (if (and (nil? c1) (nil? c2))
    (attr "class" nil)
    (attr "class" (str c1 " " c2))))

(defn- class3 [c1 c2 c3]
  (if (and (nil? c1) (nil? c2) (nil? c3))
    (attr "class" nil)
    (attr "class" (str c1 " " c2 " " c3))))

(defn- classn [& classes]
  (assert (> (count classes) 1))
  (if (every? nil? classes)
    (attr "class" nil)
    (attr "class" (reduce #(str %1 " " %2) classes))))

(defn- class-static1 [c1]
  (attr-static "class" c1))

(defn- class-static2 [c1 c2]
  (if (and (nil? c1) (nil? c2))
    (attr-static "class" nil)
    (attr-static "class" (str c1 " " c2))))

(defn- class-static3 [c1 c2 c3]
  (if (and (nil? c1) (nil? c2) (nil? c3))
    (attr-static "class" nil)
    (attr-static "class" (str c1 " " c2 " " c3))))

(defn- classn-static [& classes]
  (assert (> (count classes) 1))
  (if (every? nil? classes)
    (attr-static "class" nil)
    (attr-static "class" (reduce #(str %1 " " %2) classes))))

(defn- style [key val]
  (attr-impl key (str val) index-styles-count index-styles set-style remove-style))

(defn- style-static [key val]
  (when (and *new-node* (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-style node key (str val)))))

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
              *moved-vnode* nil]
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
              *moved-vnode* nil]
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

(defn did-move? []
  (boolean *moved-vnode*))
