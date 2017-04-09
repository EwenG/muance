(ns muance.print
  (:require [goog.object :as o] 
            [muance.core :as m :include-macros]))

(def vnode-keys [:typeid :parent :node :component :chidren-count :children
                 :attrs :unmount :key :key-moved :keymap :keymap-invalid])
(def comp-keys (-> vnode-keys (assoc 2 :comp-data) (assoc 3 :props) (assoc 6 :state)))
(def vnode-keys-text [:typeid :parent :node :text])
(def comp-data-keys [:component-name :state-ref :svg-namespace :index-in-parent
                     :component-depth :dirty-flag])

(deftype Parent [typeid])
(deftype Component [typeid])

(extend-protocol IPrintWithWriter
  Parent
  (-pr-writer [p writer opts]
    (-write writer "#parent[")
    (-write writer (if (.-typeid p) (str (.-typeid p)) "nil"))
    (-write writer "]")))

(defn with-namespace [vnode-map vnode]
  (if (m/component? vnode)
    vnode-map
    (assoc vnode-map :namespaceURI (.-namespaceURI (aget vnode m/index-node)))))

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
    (let [vnode-keys (cond (= 0 (aget vnode m/index-typeid))
                           vnode-keys-text
                           (m/component? vnode)
                           comp-keys
                           :else vnode-keys)
          l (.-length vnode)]
      (loop [m (transient {})
             i 0]
        (if (< i l)
          (let [val (cond (= i m/index-parent-vnode)
                          (when-let [p (aget vnode m/index-parent-vnode)]
                            (Parent. (aget p m/index-typeid)))
                          (and (not (m/component? vnode)) (= i m/index-component))
                          (when-let [c (aget vnode m/index-component)]
                            (Component. (aget c m/index-typeid)))
                          (= i m/index-keymap)
                          (when-let [keymap (aget vnode m/index-keymap)]
                            (into #{} (o/getKeys keymap)))
                          (and (m/component? vnode) (= i m/index-comp-data))
                          (do
                            (prn (nil? (aget vnode i)))
                            (format-comp-data (aget vnode i)))
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
    (if (= 0 (aget vnode-map m/index-typeid))
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
  (format-vnodes (.-vnode vtree)))

(defn format-render-queue [vtree]
  (when-let [render-queue (.-render-queue vtree)]
    (let [l (.-length render-queue)]
      (loop [arr (transient [])
             i 0]
        (cond (< i m/index-render-queue-offset)
              (recur (conj! arr (aget render-queue i)) (inc i))
              (< i l) (recur (conj! arr (format-depth (aget render-queue i))) (inc i))
              :else (persistent! arr))))))
