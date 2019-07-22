(ns muance.print
  (:require [goog.object :as o] 
            [muance.diff :as diff]))

(deftype Parent [typeid])
(deftype Component [typeid])
(deftype KeyedNextVnode [typeid])

(extend-protocol IPrintWithWriter
  Parent
  (-pr-writer [p writer opts]
    (-write writer "#Parent[")
    (-write writer (if (.-typeid p) (str (.-typeid p)) "nil"))
    (-write writer "]"))
  Component
  (-pr-writer [p writer opts]
    (-write writer "#Component[")
    (-write writer (if (.-typeid p) (str (.-typeid p)) "nil"))
    (-write writer "]"))
  KeyedNextVnode
  (-pr-writer [p writer opts]
    (-write writer "#KeyedNextVnode[")
    (-write writer (if (.-typeid p) (str (.-typeid p)) "nil"))
    (-write writer "]")))

(defn with-namespace [vnode-map vnode]
  (if (diff/component? vnode)
    vnode-map
    (let [node (.-nodeOrCompData vnode)]
      (assoc vnode-map :namespaceURI (.-namespaceURI node)))))

(defn format-comp-data [comp-data]
  {:compDataName (.-compDataName comp-data)
   :compDataStateRef (.-compDataStateRef comp-data)
   :compDataSvgNamespace (.-compDataSvgNamespace comp-data)
   :compDataIndexInParent (.-compDataIndexInParent comp-data)
   :compDataDepth (.-compDataDepth comp-data)
   :compDataDirtyFlag (.-compDataDirtyFlag comp-data)
   :compDataRenderedFlag (.-compDataRenderedFlag comp-data)})

(defn format-vnode [vnode]
  (when vnode
    (cond (= 0 (.-typeid vnode))
          {:typeid (.-typeid vnode)
           :parentVnode (when-let [p (.-parentVnode vnode)]
                          (->Parent (.-typeid p)))
           :nodeOrCompData (.-nodeOrCompData vnode)
           :text (.-text vnode)}
          (diff/component? vnode)
          {:typeid (.-typeid vnode)
           :parentVnode (when-let [p (.-parentVnode vnode)]
                          (->Parent (.-typeid p)))
           :nodeOrCompData (format-comp-data (.-nodeOrCompData vnode))
           :componentOrCompProps (.-componentOrCompProps vnode)
           :childrenCount (.-childrenCount vnode)
           :children (.-children vnode)
           :attrsOrCompState (.-attrsOrCompState vnode)
           :userData (.-userData vnode)
           :unmount (.-unmount vnode)
           :removeHook (.-removeHook vnode)
           :key (.-key vnode)
           :keyMoved (.-keyMoved vnode)
           :keyNextVnode (when-let [node (.-keyNextVnode vnode)]
                           (->KeyedNextVnode (.-typeid node)))
           :keymap (when-let [keymap (.-keymap vnode)]
                     (into #{} (o/getKeys keymap)))
           :keymapInvalid (.-keymapInvalid vnode)}
          :else
          {:typeid (.-typeid vnode)
           :parentVnode (when-let [p (.-parentVnode vnode)]
                          (->Parent (.-typeid p)))
           :nodeOrCompData (.-nodeOrCompData vnode)
           :componentOrCompProps (when-let [c (.-componentOrCompProps vnode)]
                                   (->Component (.-typeid c)))
           :childrenCount (.-childrenCount vnode)
           :children (.-children vnode)
           :attrsOrCompState (.-attrsOrCompState vnode)
           :userData (.-userData vnode)
           :unmount (.-unmount vnode)
           :removeHook (.-removeHook vnode)
           :key (.-key vnode)
           :keyMoved (.-keyMoved vnode)
           :keyNextVnode (when-let [node (.-keyNextVnode vnode)]
                           (->KeyedNextVnode (.-typeid node)))
           :keymap (when-let [keymap (.-keymap vnode)]
                     (into #{} (o/getKeys keymap)))
           :keymapInvalid (.-keymapInvalid vnode)})))

(declare format-vnodes)

(defn format-children [children]
  (when children
    (let [l (.-length children)]
      (loop [arr (transient [])
             i 0]
        (if (< i l)
          (recur (conj! arr (format-vnodes (aget children i)))
                 (inc i))
          (persistent! arr))))))

(defn format-vnodes [vnode]
  (let [vnode-map (format-vnode vnode)
        vnode-map (with-namespace vnode-map vnode)]
    (if (= 0 (.-typeid vnode-map))
      vnode-map
      (if (contains? vnode-map :children)
        (update-in vnode-map [:children] format-children)
        vnode-map))))

(defn format-vtree [vtree]
  (when vtree
    (format-vnodes (.-vnode vtree))))

(defn format-dirty-comp [dirty-comp]
  (when dirty-comp
    {:postRenderFn (.-postRenderFn dirty-comp)
     :props (.-props dirty-comp)
     :compFn (.-compFn dirty-comp)
     :vnode (.-vnode dirty-comp)}))

(defn format-dirty-comps-at-depth [dirty-comps]
  (when dirty-comps
    (let [l (.-length dirty-comps)]
      (loop [arr (transient [])
             i 0]
        (if (< i l)
          (let [dirty-comp (aget dirty-comps i)]
            (recur
             (conj! arr (format-dirty-comp dirty-comp))
             (inc i)))
          (persistent! arr))))))

(defn format-dirty-comps [dirty-comps]
  (when dirty-comps
    (let [l (.-length dirty-comps)]
      (loop [arr (transient [])
             i 0]
        (if (< i l)
          (if (= i 0)
            (recur
             (conj! arr (format-dirty-comp (aget dirty-comps i)))
             (inc i))
            (recur
             (conj! arr (format-dirty-comps-at-depth (aget dirty-comps i)))
             (inc i)))
          (persistent! arr))))))

(defn format-render-queue [vtree]
  (when-let [render-queue (.-render-queue vtree)]
    {:renderQueueFn (.-renderQueueFn render-queue)
     :synchronous (.-synchronous render-queue)
     :processingFlag (.-processingFlag render-queue)
     :pendingFlag (.-pendingFlag render-queue)
     :dirtyFlag (.-dirtyFlag render-queue)
     :firstRenderPromise (.-firstRenderPromise render-queue)
     :postRenderHooks (.-postRenderHooks render-queue)
     :dirtyComps (format-dirty-comps (.-dirtyComps render-queue))}))
