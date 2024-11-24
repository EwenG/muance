(ns muance.dom
  (:require [muance.diff :as diff]
            [muance.vtree :as vtree]
            [muance.context :as context]
            [muance.core :as core :include-macros true]
            [goog.object :as o]
            [muance.internal.dom]))

(def ^:const svg-ns "http://www.w3.org/2000/svg")
(def ^:const xml-ns "http://www.w3.org/XML/1998/namespace")
(def ^:const xlink-ns "http://www.w3.org/1999/xlink")

;; An empty comp useful to unmount vtree
(core/defcomp empty-comp [])

(deftype DOMVTree [id vnode render-queue]
  vtree/VTree
  (vtree/id [this] id)
  (vtree/vnode [this] vnode)
  (vtree/render-queue [this] render-queue)
  core/VTree
  (core/remove [vtree]
    (let [vnode (vtree/vnode vtree)
          fragment (.createDocumentFragment js/document)]
      (when-let [comp (aget (o/get vnode "children") 0)]
        (diff/insert-vnode-before* fragment comp nil))
      (o/set vnode "nodeOrCompData" fragment)
      (o/remove diff/roots (vtree/id vtree))))
  (core/unmount [this]
    (core/patch this empty-comp))
  (core/refresh [vtree id]
    (assert (nil? diff/*vnode*)
            "Cannot call muance.core/refresh inside render loop")
    (let [vnode (vtree/vnode vtree)
          the-render-queue (vtree/render-queue vtree)
          children (o/get vnode "children")]
      (when-let [comp (aget children 0)]
        (diff/patch-impl the-render-queue vnode comp
                         (diff/get-comp-render-fn comp)
                         (o/get comp "componentOrCompProps")
                         true)
        (diff/process-post-render-hooks the-render-queue)))))

(defn refresh-roots []
  (o/forEach diff/roots core/refresh))

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
  (when-let [p (o/get node "parentNode")]
    (.removeChild p node)))

(extend-type js/Node
  context/Context
  (context/insert-before [parent-node vnode ref-node]
    (.insertBefore parent-node (o/get vnode "nodeOrCompData") ref-node))
  (context/remove-node [parent-node node]
    (.removeChild parent-node node))
  core/VTreeInsert
  (core/insert-before [ref-node vtree]
    (let [parent-node (o/get ref-node "parentNode")
          vnode (vtree/vnode vtree)]
      (when-let [comp (aget (o/get vnode "children") 0)]
        (diff/insert-vnode-before* parent-node comp ref-node))
      (o/set vnode "nodeOrCompData" parent-node)
      (o/set diff/roots (vtree/id vtree) vtree)))
  (core/append-child [parent-node vtree]
    (let [vnode (vtree/vnode vtree)]
      (when-let [comp (aget (o/get vnode "children") 0)]
        (diff/insert-vnode-before* parent-node comp nil))
      (o/set vnode "nodeOrCompData" parent-node)
      (o/set diff/roots (vtree/id vtree) vtree))))

(defn- new-root-vnode []
  #js {:typeid nil
       :parentVnode nil
       ;; node or component field
       :nodeOrCompData (.createDocumentFragment js/document)
       ;; node or component field
       :componentOrCompProps nil
       :childrenCount 0
       :children #js []
       ;; node or component field
       :attrsOrCompState nil
       :userData nil
       ;; Unmount is stored on the node since it must be called when one of the parents
       ;; of the node is removed
       :unmount nil
       :removeHook nil
       :key nil
       ;; A slot which stores one of two flags:
       ;; - moved-flag
       ;; - moving-flag
       ;; - new-flag
       ;; See the documentation for these two flags for more details
       :keyMoved nil
       ;; keep track of the vnode sibling in order to reorder keyed vnodes during child
       ;; nodes reconciliation
       :keyNextVnode nil
       :keymap nil
       ;; When a keyed node is removed, the keymap is marked as invalid. Invalid keymaps
       ;; are cleaned when the close function of the node is called
       :keymapInvalid nil})

(defn- handle-component-update [in]
  (let [render-queue (o/get in "renderQueue")
        _ (assert (not (identical? render-queue diff/*render-queue*)))
        props (o/get in "props")
        comp-fn (o/get in "compFn")
        vnode (o/get in "vnode")
        ;; depth == -1 means this was a call to muance.core/patch. In this case, the vnode is
        ;; the vtree vnode. We don't directly pass the vnode of the component at depth 0 because
        ;; it is nil before the first rendering and this would cause potential concurrency
        ;; (multiple threads) problems
        depth (o/get in "depth")
        post-render-fn (o/get in "postRenderFn")
        synchronous? (o/get render-queue "synchronous")
        processing-flag (o/get render-queue "processingFlag")
        dirty-flag (o/get render-queue "dirtyFlag")
        first-render-promise (o/get render-queue "firstRenderPromise")]
    ;; if this is the first render
    (if (not first-render-promise)
      ;; first render is synchronous
      (do
        (diff/patch-impl render-queue vnode nil comp-fn props false)
        (diff/process-post-render-hooks render-queue)
        (o/set render-queue "firstRenderPromise" true))
      ;; if the patch data is coming from a call to the patch fn
      (do
        (if (= depth -1)
          (if-let [dirty-comps (aget (o/get render-queue "dirtyComps") 0)]
            (do
              (o/set dirty-comps "postRenderFn" post-render-fn)
              (o/set dirty-comps "props" props)
              (o/set dirty-comps "compFn" comp-fn)
              (o/set dirty-comps "vnode" vnode))
            (do
              (o/set render-queue "dirtyComps" #js [#js {:postRenderFn post-render-fn
                                                         :props props
                                                         :compFn comp-fn
                                                         :vnode vnode}])))
          (let [comp-data (o/get vnode "nodeOrCompData")]
            (when-not (identical? dirty-flag (o/get comp-data "compDataDirtyFlag"))
              (if-let [dirty-comps (aget (o/get render-queue "dirtyComps") (inc depth))]
                (do (.push dirty-comps #js {:postRenderFn post-render-fn
                                            :props props
                                            :compFn comp-fn
                                            :vnode vnode}))
                (aset (o/get render-queue "dirtyComps") (inc depth)
                      #js [#js {:postRenderFn post-render-fn
                                :props props
                                :compFn comp-fn
                                :vnode vnode}]))
              (o/set comp-data "compDataDirtyFlag" dirty-flag))))
        (o/set render-queue "pendingFlag" true)
        (if synchronous?
          (binding [diff/*rendered-flag* (js/Object.)]
            (diff/process-render-queue render-queue render-queue)
            (diff/process-post-render-hooks render-queue)
            (o/set render-queue "processingFlag" false)
            (o/set render-queue "dirtyFlag" (js/Object.)))
          (when-not (o/get render-queue "processingFlag")
            (o/set render-queue "processingFlag" true)
            (.requestAnimationFrame
             js/window 
             (fn []
               (binding [diff/*rendered-flag* (js/Object.)]
                 (o/set render-queue "processingFlag" false)
                 (o/set render-queue "dirtyFlag" (js/Object.))
                 (diff/process-render-queue render-queue render-queue)
                 (diff/process-post-render-hooks render-queue))))))))))

(defn vtree
  ([]
   (vtree nil))
  ([{:keys [synchronous? post-render-hook]}]
   (let [vt (->DOMVTree (swap! diff/vtree-ids inc)
                        (new-root-vnode)
                        #js {:renderQueueFn #'handle-component-update
                             :synchronous synchronous?
                             ;; Whether the rendering thread is still rendering dirty components
                             ;; or not. Must only be modified by the batching thread
                             :processingFlag false
                             ;; Whether dirty comps have been enqueued by the batching thread,
                             ;; waiting to be processed by the rendering thread. Must only be
                             ;; modified by the batching thread.
                             :pendingFlag false
                             ;; A flag used to know when a component has already be enqueued by
                             ;; the batching thread. Used to avoid to enqueue a component twice.
                             ;; Must only be modified by the batching thread. This flag is set on
                             ;; the component data under the compDataDirtyFlag key
                             :dirtyFlag #js {}
                             :firstRenderPromise false
                             ;; The post render hooks. Must only be modified by the rendering thread.
                             :postRenderHooks #js []
                             ;; A component is enqueued by the batching thread in this array when it
                             ;; becomes dirty. Dirty components are reset to nil before being passed
                             ;; to the rendering thread. The dirty comps used by the rendering thread
                             ;; are a flat copy (not deep copy !) of the dirty comps. Thus the
                             ;; batching thread and the rendering thread do not share the same array.
                             ;; They can both mutate this array.
                             :dirtyComps #js []})]
     (when post-render-hook
       (-> (vtree/render-queue vt)
           (o/get "postRenderHooks")
           (aset 0 (partial post-render-hook vt))))
     vt)))

(defn set-timeout
  "Execute f after a delay expressed in milliseconds. The first argument of f is the local state reference of the current component."
  [f millis]
  (assert (not (nil? diff/*vnode*))
          (str "muance.dom/set-timeout was called outside of render loop"))
  (let [component (if (diff/component? diff/*vnode*)
                    diff/*vnode*
                    (o/get diff/*vnode* "componentOrCompProps"))
        state-ref (o/get (o/get component "nodeOrCompData") "compDataStateRef")]
    (.setTimeout js/window (fn [] (f state-ref)) millis)))

(defn set-interval
  "Periodically execute f. The period is expressed in milliseconds. The first argument of f is the local state reference of the current component."
  [f millis]
  (assert (not (nil? diff/*vnode*))
          (str "muance.dom/set-interval was called outside of render loop"))
  (let [component (if (diff/component? diff/*vnode*)
                    diff/*vnode*
                    (o/get diff/*vnode* "componentOrCompProps"))
        state-ref (o/get (o/get component "nodeOrCompData") "compDataStateRef")]
    (.setInterval js/window (fn [] (f state-ref)) millis)))

