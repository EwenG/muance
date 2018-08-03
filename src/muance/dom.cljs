(ns muance.dom
  (:require [muance.diff :as diff]
            [muance.vtree :as vtree]
            [muance.context :as context]
            [muance.core :as core]
            [goog.object :as o]))

(def ^:const svg-ns "http://www.w3.org/2000/svg")
(def ^:const xml-ns "http://www.w3.org/XML/1998/namespace")
(def ^:const xlink-ns "http://www.w3.org/1999/xlink")

(defn- new-text-vnode [element text]
     #js [0 diff/*vnode* element text])

(defn- text-node [t]
  (let [vnode-index (or (aget diff/*vnode* diff/index-children-count) 0)
        parent-children (or (aget diff/*vnode* diff/index-children) #js [])
        prev (aget parent-children vnode-index)
        prev-key (when prev (aget prev diff/index-key))
        prev-typeid (when prev (aget prev diff/index-typeid))]
    (aset diff/*vnode* diff/index-children-count (inc vnode-index))
    (when (nil? (aget diff/*vnode* diff/index-children))
      (aset diff/*vnode* diff/index-children parent-children))
    (if (= 0 prev-typeid)
      (when (not= (aget prev diff/index-text) t)
        (aset prev diff/index-text t)
        (o/set (aget prev diff/index-node) "nodeValue" t))
      (let [vnode (new-text-vnode (.createTextNode js/document t) t)]
        (diff/insert-vnode-before
         diff/*vnode* vnode (aget parent-children vnode-index) vnode-index)
        (aset parent-children vnode-index vnode)
        (if prev-key
          (diff/remove-vnode-key prev)
          (when prev (diff/remove-vnode prev)))))))

(defn handle-event-handler [key prev-handler handler]
  (let [node (aget diff/*vnode* diff/index-node)]
    (when prev-handler
      (.removeEventListener node key prev-handler false))
    (when handler
      (.addEventListener node key handler false)))
  handler)

(defn- set-attribute [ns key val]
  (let [node (aget diff/*vnode* diff/index-node)]
    (if (nil? val)
      (.removeAttribute node key)
      (if (nil? ns)
        (.setAttribute node key val)
        (.setAttributeNS node ns key val)))))

(defn- set-property [key val]
  (let [node (aget diff/*vnode* diff/index-node)]
    (o/set node key val)))

(defn- set-input-value [val]
  (let [node (aget diff/*vnode* diff/index-node)]
    (when (not= (o/get node "value") val)
      (o/set node "value" val))))

(defn- set-style [key val]
  (let [node (aget diff/*vnode* diff/index-node)]
    (o/set (.-style node) key val)))

(defn- set-style-custom [key val]
  (let [node (aget diff/*vnode* diff/index-node)]
    (.setProperty (.-style node) key val)))

(defn make-handler-0 [f]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref)))))

(defn- on-0 [key f]
  (when (diff/compare-handlers-0 f)
    (diff/set-handler-0
     (handle-event-handler key diff/*handlers-prev* (make-handler-0 f)) f))
  (diff/inc-attrs 2))

(defn- on-static-0 [key f]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-0 f))))

(defn make-handler-1 [f arg1]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref arg1)))))

(defn- on-1 [key f arg1]
  (when (diff/compare-handlers-1 f arg1)
    (diff/set-handler-1
     (handle-event-handler key diff/*handlers-prev* (make-handler-1 f arg1)) f arg1))
  (diff/inc-attrs 3))

(defn- on-static-1 [key f arg1]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-1 f arg1))))

(defn make-handler-2 [f arg1 arg2]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref arg1 arg2)))))

(defn- on-2 [key f arg1 arg2]
  (when (diff/compare-handlers-2 f arg1 arg2)
    (diff/set-handler-2
     (handle-event-handler key diff/*handlers-prev* (make-handler-2 f arg1 arg2))
     f arg1 arg2))
  (diff/inc-attrs 4))

(defn- on-static-2 [key f arg1 arg2]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-2 f arg1 arg2))))

(defn make-handler-3 [f arg1 arg2 arg3]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (fn [e] (f e state-ref arg1 arg2 arg3)))))

(defn- on-3 [key f arg1 arg2 arg3]
  (when (diff/compare-handlers-3 f arg1 arg2 arg3)
    (diff/set-handler-3
     (handle-event-handler key diff/*handlers-prev* (make-handler-3 f arg1 arg2 arg3))
     f arg1 arg2 arg3))
  (diff/inc-attrs 5))

(defn- on-static-3 [key f arg1 arg2 arg3]
  (when (diff/compare-handlers-static f)
    (handle-event-handler key nil (make-handler-3 f arg1 arg2 arg3))))

(defn nil-or-string [v]
  (if (nil? v) nil (str v)))

(defn- attr-ns [ns key val]
  (let [val (nil-or-string val)
        changed? (diff/compare-attrs val)]
    (when changed?
      (set-attribute ns key val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn- attr-ns-static [ns key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (set-attribute ns key (str val))))

(defn- prop [key val]
  (if (> diff/*svg-namespace* 0)
    (let [val (nil-or-string val)]
      (when (diff/compare-attrs val)
        (set-attribute nil key val)
        (diff/set-attr val)))
    (when (diff/compare-attrs val)
      (set-property key val)
      (diff/set-attr val)))
  (diff/inc-attrs 1))

(defn- prop-static [key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (if (> diff/*svg-namespace* 0)
      (set-attribute nil key (str val))
      (set-property key val))))

(defn- input-value [val]
  (let [val (nil-or-string val)]
    (when (diff/compare-attrs val)
      (set-input-value val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn- style [key val]
  (let [val (str val)]
    (when (diff/compare-attrs val)
      (set-style key val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn- style-static [key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (set-style key (str val))))

(defn- style-custom [key val]
  (let [val (str val)]
    (when (diff/compare-attrs val)
      (set-style-custom key val)
      (diff/set-attr val))
    (diff/inc-attrs 1)))

(defn- style-custom-static [key val]
  (when (and (> diff/*new-node* 0) (not (nil? val)))
    (set-style-custom key (str val))))

(deftype DOMVTree [id vnode render-queue]
  vtree/VTree
  (vtree/id [this] id)
  (vtree/vnode [this] vnode)
  (vtree/render-queue [this] render-queue)
  (vtree/synchronous-first-render [this] true)
  core/VTree
  (core/remove [vtree]
    (let [vnode (vtree/vnode vtree)
          fragment (.createDocumentFragment js/document)]
      (when-let [comp (aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* fragment comp nil))
      (aset vnode diff/index-node fragment)
      (o/remove diff/roots (vtree/id vtree)))))

(extend-protocol context/CreateElement
  nil
  (create-element [tag]
    ;; tag is nil when opening a component
    nil)
  string
  (create-element [tag]
    (if (> diff/*svg-namespace* 0)
      (.createElementNS js/document svg-ns tag)
      (.createElement js/document tag))))

(defn remove-node [node]
  (when-let [p (.-parentNode node)]
    (.removeChild p node)))

(extend-type js/Node
  context/Context
  (context/insert-before [parent-node vnode ref-node]
    (.insertBefore parent-node (aget vnode diff/index-node) ref-node))
  (context/remove-node [parent-node node]
    (.removeChild parent-node node))
  core/VTreeInsert
  (core/insert-before [ref-node vtree]
    (let [parent-node (.-parentNode ref-node)
          vnode (vtree/vnode vtree)]
      (when-let [comp (aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* parent-node comp ref-node))
      (aset vnode diff/index-node parent-node)
      (o/set diff/roots (vtree/id vtree) vtree)))
  (core/append-child [parent-node vtree]
    (let [vnode (vtree/vnode vtree)]
      (when-let [comp (aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* parent-node comp nil))
      (aset vnode diff/index-node parent-node)
      (o/set diff/roots (vtree/id vtree) vtree))))

(defn- new-root-vnode []
  #js [nil nil (.createDocumentFragment js/document) nil 0 #js []])

(defn async-fn [f]
  (.requestAnimationFrame js/window f))

(defn vtree
  ([]
   (vtree nil))
  ([{:keys [async]}]
   (->DOMVTree (swap! diff/vtree-ids inc)
               (new-root-vnode)
               ;; async-fn + post render hooks + internal post render hooks
               #js [(when async async-fn) #js [] #js []])))

(defn set-timeout
  "Execute f after a delay expressed in milliseconds. The first argument of f is the local state reference of the vnode component."
  [vnode f millis]
  (assert vnode "muance.core/set-timeout expects a vnode.")
  (let [component (if (diff/component? vnode) vnode (aget vnode diff/index-component))
        state-ref (aget component diff/index-comp-data diff/index-comp-data-state-ref)]
    (.setTimeout js/window (fn [] (f state-ref)) millis)))

(defn set-interval
  "Periodically execute f. The period is expressed in milliseconds. The first argument of f is the local state reference of the vnode component."
  [vnode f millis]
  (assert vnode "muance.core/set-timeout expects a vnode.")
  (let [component (if (diff/component? vnode) vnode (aget vnode diff/index-component))
        state-ref (aget component diff/index-comp-data diff/index-comp-data-state-ref)]
    (.setInterval js/window (fn [] (f state-ref)) millis)))

#_(def dom-context #js {:insert-fn insert-fn-dom
                        :remove-node-fn remove-node
                        :create-element-fn create-element
                        :handle-event-handler-fn handle-event-handler
                        :make-handler-0 make-handler-fn-0
                        :make-handler-1 make-handler-fn-1
                        :make-handler-2 make-handler-fn-2
                        :make-handler-3 make-handler-fn-3
                        :async-fn async-fn
                        :patch-fn patch-impl})
