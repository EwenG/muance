(ns muance.print
  (:require [goog.object :as o] 
            [muance.diff :as diff]))

(def vnode-keys [:typeid :parent :node :component :chidren-count :children
                 :attrs :user-data :unmount :remove-hook :key :key-moved :keyed-next-vnode
                 :keymap :keymap-invalid])
(def comp-keys (-> vnode-keys (assoc 2 :comp-data) (assoc 3 :props) (assoc 6 :state)))
(def vnode-keys-text [:typeid :parent :node :text])
(def comp-data-keys [:component-name :state-ref :svg-namespace :index-in-parent
                     :component-depth :dirty-flag :rendered-flag])

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
    (let [node (aget vnode diff/index-node)]
      (assoc vnode-map :namespaceURI (.-namespaceURI node)))))

(defn format-comp-data [comp-data]
  (let [l (.-length comp-data)]
    (loop [m (transient {})
           i 0]
      (if (< i l)
        (recur (assoc! m (get comp-data-keys i) (aget comp-data i))
               (inc i))
        (persistent! m)))))

(defn format-vnode [vnode]
  (when vnode
    (let [vnode-keys (cond (= 0 (aget vnode diff/index-typeid))
                           vnode-keys-text
                           (diff/component? vnode)
                           comp-keys
                           :else vnode-keys)
          l (.-length vnode)]
      (loop [m (transient {})
             i 0]
        (if (< i l)
          (let [val (cond (= i diff/index-parent-vnode)
                          (when-let [p (aget vnode diff/index-parent-vnode)]
                            (->Parent (aget p diff/index-typeid)))
                          (and (not (diff/component? vnode)) (= i diff/index-component))
                          (when-let [c (aget vnode diff/index-component)]
                            (->Component (aget c diff/index-typeid)))
                          (= i diff/index-key-next-vnode)
                          (when-let [node (aget vnode diff/index-key-next-vnode)]
                            (->KeyedNextVnode (aget node diff/index-typeid)))
                          (= i diff/index-keymap)
                          (when-let [keymap (aget vnode diff/index-keymap)]
                            (into #{} (o/getKeys keymap)))
                          (and (diff/component? vnode) (= i diff/index-comp-data))
                          (format-comp-data (aget vnode i))
                          :else (aget vnode i))]
            (recur (assoc! m (get vnode-keys i) val)
                   (inc i)))
          (persistent! m))))))

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
    (if (= 0 (get vnode-map diff/index-typeid))
      vnode-map
      (if (contains? vnode-map :children)
        (update-in vnode-map [:children] format-children)
        vnode-map))))

(defn format-depth [dirty-comps]
  (when dirty-comps
    (let [l (.-length dirty-comps)]
      (loop [arr (transient [])
             i 0]
        (if (< i l)
          (recur
           (-> arr
               (conj! (aget dirty-comps i))
               (conj! (aget dirty-comps (inc i)))
               (conj! (format-vnodes (aget dirty-comps (+ i 2)))))
           (+ i 3))
          (persistent! arr))))))

(defn format-vtree [vtree]
  (when vtree
    (format-vnodes (.-vnode vtree))))

(defn format-render-queue [vtree]
  (when-let [render-queue (.-render-queue vtree)]
    (let [l (.-length render-queue)]
      (loop [arr (transient [])
             i 0]
        (cond (< i diff/index-render-queue-offset)
              (recur (conj! arr (aget render-queue i)) (inc i))
              (< i l) (recur (conj! arr (format-depth (aget render-queue i))) (inc i))
              :else (persistent! arr))))))
