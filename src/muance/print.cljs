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
    (-write writer (if (o/get p "typeid") (str (o/get p "typeid")) "nil"))
    (-write writer "]"))
  Component
  (-pr-writer [p writer opts]
    (-write writer "#Component[")
    (-write writer (if (o/get p "typeid") (str (o/get p "typeid")) "nil"))
    (-write writer "]"))
  KeyedNextVnode
  (-pr-writer [p writer opts]
    (-write writer "#KeyedNextVnode[")
    (-write writer (if (o/get p "typeid") (str (o/get p "typeid")) "nil"))
    (-write writer "]")))

(defn with-namespace [vnode-map vnode]
  (if (diff/component? vnode)
    vnode-map
    (let [node (o/get vnode "nodeOrCompData")]
      (assoc vnode-map :namespaceURI (o/get node "namespaceURI")))))

(defn format-comp-data [comp-data]
  {:compDataName (o/get comp-data "compDataName")
   :compDataStateRef (o/get comp-data "compDataStateRef")
   :compDataSvgNamespace (o/get comp-data "compDataSvgNamespace")
   :compDataIndexInParent (o/get comp-data "compDataIndexInParent")
   :compDataDepth (o/get comp-data "compDataDepth")
   :compDataDirtyFlag (o/get comp-data "compDataDirtyFlag")
   :compDataRenderedFlag (o/get comp-data "compDataRenderedFlag")})

(defn format-vnode [vnode]
  (when vnode
    (cond (= 0 (o/get vnode "typeid"))
          {:typeid (o/get vnode "typeid")
           :parentVnode (when-let [p (o/get vnode "parentVnode")]
                          (->Parent (o/get p "typeid")))
           :nodeOrCompData (o/get vnode "nodeOrCompData")
           :text (o/get vnode "text")}
          (diff/component? vnode)
          {:typeid (o/get vnode "typeid")
           :parentVnode (when-let [p (o/get vnode "parentVnode")]
                          (->Parent (o/get p "typeid")))
           :nodeOrCompData (format-comp-data (o/get vnode "nodeOrCompData"))
           :componentOrCompProps (o/get vnode "componentOrCompProps")
           :childrenCount (o/get vnode "childrenCount")
           :children (o/get vnode "children")
           :attrsOrCompState (o/get vnode "attrsOrCompState")
           :userData (o/get vnode "userData")
           :unmount (o/get vnode "unmount")
           :removeHook (o/get vnode "removeHook")
           :key (o/get vnode "key")
           :keyMoved (o/get vnode "keyMoved")
           :keyNextVnode (when-let [node (o/get vnode "keyNextVnode")]
                           (->KeyedNextVnode (o/get node "typeid")))
           :keymap (when-let [keymap (o/get vnode "keymap")]
                     (into #{} (o/getKeys keymap)))
           :keymapInvalid (o/get vnode "keymapInvalid")}
          :else
          {:typeid (o/get vnode "typeid")
           :parentVnode (when-let [p (o/get vnode "parentVnode")]
                          (->Parent (o/get p "typeid")))
           :nodeOrCompData (o/get vnode "nodeOrCompData")
           :componentOrCompProps (when-let [c (o/get vnode "componentOrCompProps")]
                                   (->Component (o/get c "typeid")))
           :childrenCount (o/get vnode "childrenCount")
           :children (o/get vnode "children")
           :attrsOrCompState (o/get vnode "attrsOrCompState")
           :userData (o/get vnode "userData")
           :unmount (o/get vnode "unmount")
           :removeHook (o/get vnode "removeHook")
           :key (o/get vnode "key")
           :keyMoved (o/get vnode "keyMoved")
           :keyNextVnode (when-let [node (o/get vnode "keyNextVnode")]
                           (->KeyedNextVnode (o/get node "typeid")))
           :keymap (when-let [keymap (o/get vnode "keymap")]
                     (into #{} (o/getKeys keymap)))
           :keymapInvalid (o/get vnode "keymapInvalid")})))

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
    (if (= 0 (o/get vnode-map "typeid"))
      vnode-map
      (if (contains? vnode-map :children)
        (update-in vnode-map [:children] format-children)
        vnode-map))))

(defn format-vtree [vtree]
  (when vtree
    (format-vnodes (o/get vtree "vnode"))))

(defn format-dirty-comp [dirty-comp]
  (when dirty-comp
    {:postRenderFn (o/get dirty-comp "postRenderFn")
     :props (o/get dirty-comp "props")
     :compFn (o/get dirty-comp "compFn")
     :vnode (o/get dirty-comp "vnode")}))

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
  (when-let [render-queue (o/get vtree "render-queue")]
    {:renderQueueFn (o/get render-queue "renderQueueFn")
     :synchronous (o/get render-queue "synchronous")
     :processingFlag (o/get render-queue "processingFlag")
     :pendingFlag (o/get render-queue "pendingFlag")
     :dirtyFlag (o/get render-queue "dirtyFlag")
     :firstRenderPromise (o/get render-queue "firstRenderPromise")
     :postRenderHooks (o/get render-queue "postRenderHooks")
     :dirtyComps (format-dirty-comps (o/get render-queue "dirtyComps"))}))
