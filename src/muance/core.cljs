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
(def ^{:private true} index-state-ref 7)
(def ^{:private true} index-unmount 8)
(def ^{:private true} index-key 9)
(def ^{:private true} index-key-moved 10)
(def ^{:private true} index-keymap 11)
(def ^{:private true} index-keymap-invalid 12)

(def ^{:private true} index-text 3)

(def ^{:private true} index-comp-props 5)
(def ^{:private true} index-comp-state 6)

(def ^{:private true} node-data-key "muance.core/node-data")

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

(defonce ^{:private true} moved-flag nil)

(def ^:dynamic *state* nil)

(defn- remove-vnode-key [vnode key]
  (aset vnode index-parent-vnode nil)
  (aset *current-vnode* index-keymap-invalid
        (inc (aget *current-vnode* index-keymap-invalid))))

(defn- call-unmount-hooks [vnode]
  (when-let [unmount-hook (aget vnode index-unmount)]
    (set! *current-vnode* vnode)
    (unmount-hook @(aget vnode index-state-ref))))

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
  (if-let [node (aget vnode index-node)]
    (dom/removeNode node)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (remove-real-node (aget children i))
            (recur (inc i))))))))

(defn- remove-node [vnode]
  (when vnode
    (let [current-vnode *current-vnode*]
      (recursively-call-unmount-hooks vnode)
      (set! *current-vnode* current-vnode))
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

(defn- set-attribute [node key val]
  (if (nil? val)
    (.removeAttribute node key)
    (.setAttribute node key val)))

(defn- set-style [node key val]
  (o/set (.-style node) key val))

(defn- noop [vnode props state])

(defn- init-keymap [keymap]
  (aset *current-vnode* index-keymap keymap)
  (aset *current-vnode* index-keymap-invalid 0)
  keymap)

(defn- new-vnode [typeid element willUnmount]
  #js [typeid *current-vnode* element])

(defn- new-vnode-unmount [typeid element willUnmount]
  #js [typeid *current-vnode* element 0 nil 0 nil nil willUnmount])

(defn- new-vnode-key [typeid element willUnmount keymap key]
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode #js [typeid *current-vnode* element 0 nil 0 nil nil nil key moved-flag]]
    (o/set keymap key vnode)
    vnode))

(defn- new-vnode-key-unmount [typeid element willUnmount keymap key]
  (let [keymap (if (nil? keymap) (init-keymap #js {}) keymap)
        vnode #js [typeid *current-vnode* element 0 nil 0 nil nil willUnmount key moved-flag]]
    (o/set keymap key vnode)
    vnode))

(defn- new-text-vnode [element text]
  #js [-1 *current-vnode* element text])

(defn- parent-node [parent]
  (if-let [node (aget parent index-node)]
    node
    (recur (aget parent index-parent-vnode))))

(defn- ref-node [vnode]
  (if-let [node (aget vnode index-node)]
    node
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (if-let [node (ref-node (aget children i))]
              node
              (recur (inc i)))))))))

(defn- insert-before* [parent-node vnode ref-node]
  (if-let [node (aget vnode index-node)]
    (.insertBefore parent-node node ref-node)
    (when-let [children (aget vnode index-children)]
      (let [l (.-length children)]
        (loop [i 0]
          (when (< i l)
            (insert-before* parent-node (aget children i) ref-node)
            (recur (inc i))))))))

(defn- insert-before [parent-vnode vnode ref-vnode]
  (let [parent-node (parent-node parent-vnode)
        ref-node (when ref-vnode (ref-node ref-vnode))]
    (insert-before* parent-node vnode ref-node)))

(defn- splice-to [nodes index moved-node to-node]
  (let [next-moved-node (aget nodes index)]
    (aset nodes index moved-node)
    (when (and next-moved-node (not (identical? next-moved-node to-node)))
      (recur nodes (inc index) next-moved-node to-node))))

(defn- open-impl [tag typeid key willUpdate willUnmount
                  new-vnode new-vnode-key set-state-ref?]
  (let [key (when key (str key))
        vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (or (aget *current-vnode* index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))
        keymap (aget *current-vnode* index-keymap)]
    (aset *current-vnode* index-children-count (inc vnode-index))
    (when-not (aget *current-vnode* index-children)
      (aset *current-vnode* index-children parent-children))
    (if (and (= key prev-key) (= typeid prev-typeid))
      (do
        (when key
          (aset prev index-key-moved moved-flag))
        (set! *current-vnode* prev)
        (willUpdate *props* *state*))
      (let [moved-vnode (and key keymap (o/get keymap key))]
        (if (and moved-vnode
                 (= typeid (aget moved-vnode index-typeid))
                 (not (identical? moved-flag (aget moved-vnode index-key-moved))))
          (do (when (nil? *moved-vnode*)
                (set! *moved-vnode* moved-vnode))
              (willUpdate *props* *state*)
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
              (set! *current-vnode* moved-vnode))
          ;; this is a new node -> replace the node at the current index
          (let [vnode (if key
                        (new-vnode-key
                         typeid (when tag (.createElement js/document tag))
                         willUnmount keymap key)
                        (new-vnode
                         typeid (when tag (.createElement js/document tag)) willUnmount))
                ;; handle invalid states
                ;; the prev-key is refreshed since it can be modified by the invalid state
                ;; handling
                prev-key (cond (and moved-vnode
                                    (identical?
                                     moved-flag (aget moved-vnode index-key-moved)))
                               (do
                                 (.error js/console (str "Duplicate key: " key))
                                 (aset moved-vnode index-key nil)
                                 (when prev (aget prev index-key)))
                               (and moved-vnode (not= typeid (aget moved-vnode index-typeid)))
                               (do
                                 (.error
                                  js/console
                                  (str "Nodes with same key and different typeids. key: " key))
                                 (aset moved-vnode index-key nil)
                                 (when (nil? (aget moved-vnode index-parent-vnode))
                                   (aset *current-vnode* index-keymap-invalid
                                         (dec (aget *current-vnode* index-keymap-invalid)))
                                   (remove-node moved-vnode))
                                 (when prev (aget prev index-key)))
                               :else prev-key)]
            (insert-before *current-vnode* vnode (aget parent-children (inc vnode-index)))
            (aset parent-children vnode-index vnode)
            (when set-state-ref? (aset vnode index-state-ref *state-ref*))
            (set! *new-node* true)
            (if prev-key
              (remove-vnode-key prev prev-key)
              (remove-node prev))
            (set! *current-vnode* vnode)))))))

(defn- open [tag typeid]
  (open-impl tag (or typeid tag) nil noop noop new-vnode new-vnode-key false))

(defn- open-opts [tag typeid key willUpdate willUnmount]
  (if willUnmount
    (open-impl tag (or typeid tag) key
               (or willUpdate noop) willUnmount
               new-vnode-unmount new-vnode-key-unmount true)
    (open-impl tag (or typeid tag) key (or willUpdate noop)
               noop new-vnode new-vnode-key false)))

(defn- close []
  (when (aget *current-vnode* index-attrs-count)
    (aset *current-vnode* index-attrs-count 0))
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (set! *new-node* false)
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn- close-opts [didMount didUpdate]
  (when (aget *current-vnode* index-attrs-count)
    (aset *current-vnode* index-attrs-count 0))
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (if *new-node*
    (when didMount
      (set! *new-node* false)
      (.push *didMounts* didMount)
      (.push *didMounts* *props*)
      (.push *didMounts* *state*))
    (when didUpdate (didUpdate *props* *state*)))
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

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
      (let [vnode (new-text-vnode (.createTextNode js/document t) t)]
        (insert-before *current-vnode* vnode (aget parent-children vnode-index))
        (aset parent-children vnode-index vnode)
        (if prev-key
          (remove-vnode-key prev prev-key)
          (remove-node prev))))))

(defn- patch-local [vnode patch-fn props? props]
  (set! moved-flag #js [])
  (binding [*current-vnode* vnode
            *didMounts* #js []
            *new-node* false
            *props* nil
            *state* nil
            *state-ref* nil
            *moved-vnode* nil
            *skip* false]
    (let [key (aget vnode index-key)]
      (if props?
        (patch-fn key props)
        (patch-fn key)))))

(defn- open-comp* [typeid props key state-ref willUpdate willUnmount]
  (set! *props* props)
  (set! *state* @state-ref)
  (if willUnmount
    (open-impl nil typeid key (or willUpdate noop) willUnmount
               new-vnode-unmount new-vnode-key-unmount false)
    (open-impl nil typeid key (or willUpdate noop) noop
               new-vnode new-vnode-key false))
  (aset *current-vnode* index-comp-props *props*)
  (aset *current-vnode* index-comp-state @state-ref))

(defn- remove-local-watch [state]
  (remove-watch (aget *current-vnode* index-state-ref) ::component))

(defn- will-receive-props [prev-props props willReceiveProps]
  (when (and willReceiveProps (not (identical? prev-props props)))
    (willReceiveProps prev-props props *state-ref*)))

(defn- open-comp-opts [typeid props? props comp-fn key
                       willUpdate willUnmount willReceiveProps getInitialState]
  (let [vnode-index (or (aget *current-vnode* index-children-count) 0)
        parent-children (aget *current-vnode* index-children)
        prev (when parent-children (aget parent-children vnode-index))
        prev-key (when prev (aget prev index-key))
        prev-typeid (when prev (aget prev index-typeid))
        keymap (aget *current-vnode* index-keymap)
        moved-vnode (and key keymap (o/get keymap key))]
    (if (or (and (nil? key) (nil? prev-key) (= typeid prev-typeid))
            (and key (= key prev-key)))
      (let [prev-props (aget prev index-comp-props)
            prev-state (aget prev index-comp-state)
            prev-state-ref (aget prev index-state-ref)]
        (if (and (identical? prev-props props)
                 (identical? prev-state @prev-state-ref))
          (do (set! *skip* true)
              (aset prev index-children-count (inc vnode-index))
              (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))
          (do (set! *state-ref* prev-state-ref)
              (will-receive-props prev-props props willReceiveProps)
              (open-comp* typeid props key prev-state-ref willUpdate willUnmount))))
      (if moved-vnode
        (let [prev-props (aget moved-vnode index-comp-props)
              prev-state-ref (aget moved-vnode index-state-ref)]
          (do (set! *state-ref* prev-state-ref)
              (will-receive-props prev-props props willReceiveProps)
              (open-comp* typeid props key prev-state-ref willUpdate willUnmount)))
        (let [state (atom (when getInitialState (getInitialState props)))
              willUnmount (if willUnmount
                            (fn [state] (willUnmount state) (remove-local-watch state))
                            remove-local-watch)]
          (set! *state-ref* state)
          (open-comp* typeid props key state willUpdate willUnmount)
          (let [vnode *current-vnode*]
            (aset vnode index-state-ref state)
            (add-watch state ::component (fn [k r o n]
                                           (patch-local vnode comp-fn props? props)))))))))

(defn- open-comp [typeid props? props comp-fn]
  (open-comp-opts typeid props? props comp-fn nil nil nil nil nil))

(defn- close-comp []
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (set! *new-node* false)
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn- close-comp-opts [didMount didUpdate]
  (clean-children *current-vnode*)
  (clean-keymap *current-vnode*)
  (if *new-node*
    (when didMount
      (set! *new-node* false)
      (.push *didMounts* didMount)
      (.push *didMounts* *props*)
      (.push *didMounts* *state*))
    (when didUpdate (didUpdate *props* *state*)))
  (when (identical? *moved-vnode* *current-vnode*)
    (set! *moved-vnode* nil))
  (set! *current-vnode* (aget *current-vnode* index-parent-vnode)))

(defn- attr-impl [key val set-fn]
  (let [attrs-index (or (aget *current-vnode* index-attrs-count) 0)
        prev-attrs (or (aget *current-vnode* index-attrs) #js [])
        prev-val (aget prev-attrs attrs-index)
        prev-node (aget *current-vnode* index-node)]
    (when-not (aget *current-vnode* index-attrs)
      (aset *current-vnode* index-attrs prev-attrs))
    (aset *current-vnode* index-attrs-count (inc attrs-index))
    (when (not= prev-val val)
      (aset prev-attrs attrs-index val)
      (set-fn prev-node key val))))

(defn- attr [key val]
  (attr-impl key (when (not (nil? val)) (str val)) set-attribute))

(defn- attr-static [key val]
  (when (and *new-node* (not (nil? val)))
    (let [node (aget *current-vnode* index-node)]
      (set-attribute node key (str val)))))

(defn- style [key val]
  (attr-impl key (str val) set-style))

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
  (set! moved-flag #js [])
  (if-let [node-data (o/get node node-data-key)]
    (binding [*current-vnode* node-data
              *didMounts* #js []
              *new-node* false
              *props* nil
              *state* nil
              *state-ref* nil
              *moved-vnode* nil
              *skip* false]
      (if props?
        (patch-fn props)
        (patch-fn))
      (clean-children *current-vnode*)
      (clean-keymap *current-vnode*)
      (call-did-mount-hooks *didMounts*))
    (binding [*current-vnode* #js [nil nil node 0 #js []]
              *didMounts* #js []
              *new-node* false
              *props* nil
              *state* nil
              *state-ref* nil
              *moved-vnode* nil
              *skip* false]
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

