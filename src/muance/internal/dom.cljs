(ns muance.internal.dom
  (:require [muance.diff :as diff]
            [goog.object :as o]))

(defn- nil-or-string [v]
  (if (nil? v) nil (str v)))

(defn- new-text-vnode [element text]
  #js {:typeid 0
       :parentVnode diff/*vnode*
       :nodeOrCompData element
       :text text})

(defn text-node [t]
  (let [vnode-index (or (.-childrenCount diff/*vnode*) 0)
        parent-children (or (.-children diff/*vnode*) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (.-key prev))
        prev-typeid (when prev (.-typeid prev))]
    (set! (.-childrenCount diff/*vnode*) (inc vnode-index))
    (when (nil? (.-children diff/*vnode*))
      (set! (.-children diff/*vnode*) parent-children))
    (if (= 0 prev-typeid)
      (when (not= (.-text prev) t)
        (set! (.-text prev) t)
        (o/set (.-nodeOrCompData prev) "nodeValue" t))
      (let [vnode (new-text-vnode (.createTextNode js/document t) t)]
        (diff/insert-vnode-before
         diff/*vnode* vnode (aget parent-children vnode-index) vnode-index)
        (aset parent-children vnode-index vnode)
        (if prev-key
          (diff/remove-vnode-key prev)
          (when prev (diff/remove-vnode prev)))))))

(defn- handle-event-handler [key prev-handler handler]
  (let [node (.-nodeOrCompData diff/*vnode*)]
    (when prev-handler
      (.removeEventListener node key prev-handler false))
    (when handler
      (.addEventListener node key handler false)))
  handler)

(defn set-attribute [ns key val]
  (let [node (.-nodeOrCompData diff/*vnode*)]
    (if (nil? val)
      (.removeAttribute node key)
      (if (nil? ns)
        (.setAttribute node key val)
        (.setAttributeNS node ns key val)))))

(defn set-property [key val]
  (let [node (.-nodeOrCompData diff/*vnode*)]
    (o/set node key val)))

(defn- set-style-custom [key val]
  (let [node (.-nodeOrCompData diff/*vnode*)]
    (.setProperty (.-style node) key val)))

(defn make-handler-0 [f]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref)))))

(defn on-0 [key f]
  (when (diff/compare-handlers-0 f)
    (diff/set-handler-0
     (handle-event-handler key diff/*handlers-prev* (make-handler-0 f)) f))
  (diff/inc-attrs 2))

(defn on-static-0 [key f]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-0 f))))

(defn- make-handler-1 [f arg1]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref arg1)))))

(defn on-1 [key f arg1]
  (when (diff/compare-handlers-1 f arg1)
    (diff/set-handler-1
     (handle-event-handler key diff/*handlers-prev* (make-handler-1 f arg1)) f arg1))
  (diff/inc-attrs 3))

(defn on-static-1 [key f arg1]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-1 f arg1))))

(defn- make-handler-2 [f arg1 arg2]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref arg1 arg2)))))

(defn on-2 [key f arg1 arg2]
  (when (diff/compare-handlers-2 f arg1 arg2)
    (diff/set-handler-2
     (handle-event-handler key diff/*handlers-prev* (make-handler-2 f arg1 arg2))
     f arg1 arg2))
  (diff/inc-attrs 4))

(defn on-static-2 [key f arg1 arg2]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-2 f arg1 arg2))))

(defn- make-handler-3 [f arg1 arg2 arg3]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref arg1 arg2 arg3)))))

(defn on-3 [key f arg1 arg2 arg3]
  (when (diff/compare-handlers-3 f arg1 arg2 arg3)
    (diff/set-handler-3
     (handle-event-handler key diff/*handlers-prev* (make-handler-3 f arg1 arg2 arg3))
     f arg1 arg2 arg3))
  (diff/inc-attrs 5))

(defn on-static-3 [key f arg1 arg2 arg3]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-3 f arg1 arg2 arg3))))

(defn attr-ns [ns key val]
  (let [val (nil-or-string val)
        changed? (diff/compare-attrs val)]
    (when changed?
      (set-attribute ns key val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn attr-ns-static [ns key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (set-attribute ns key (str val))))

(defn prop [key val]
  (if (> diff/*svg-namespace* 0)
    (let [val (nil-or-string val)]
      (when (diff/compare-attrs val)
        (set-attribute nil key val)
        (diff/set-attr val)))
    (when (diff/compare-attrs val)
      (set-property key val)
      (diff/set-attr val)))
  (diff/inc-attrs 1))

(defn prop-static [key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (if (> diff/*svg-namespace* 0)
      (set-attribute nil key (str val))
      (set-property key val))))

(defn- set-input-value [val]
  (let [node (.-nodeOrCompData diff/*vnode*)]
    (when (not= (o/get node "value") val)
      (o/set node "value" val))))

(defn input-value [val]
  (let [val (nil-or-string val)]
    (when (diff/compare-attrs val)
      (set-input-value val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn- set-style [key val]
  (let [node (.-nodeOrCompData diff/*vnode*)]
    (o/set (.-style node) key val)))

(defn style [key val]
  (let [val (str val)]
    (when (diff/compare-attrs val)
      (set-style key val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn style-static [key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (set-style key (str val))))

(defn style-custom [key val]
  (let [val (str val)]
    (when (diff/compare-attrs val)
      (set-style-custom key val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn style-custom-static [key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (set-style-custom key (str val))))
