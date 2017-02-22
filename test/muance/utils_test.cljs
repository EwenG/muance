(ns muance.utils-test
  (:require [goog.dom :as dom]
            [goog.object :as o]
            [muance.core :as m :include-macros]))


(defn new-root []
  (when-let [root (.getElementById js/document "root")]
    (dom/removeNode root))
  (.appendChild js/document.body (doto (js/document.createElement "div")
                                   (aset "id" "root"))))

(defn root []
  (.getElementById js/document "root"))

(def vnode-keys [:typeid :parent :node :chidren-count :children :attrs-count :attrs :state-ref
                 :unmount :key :key-moved :keymap :keymap-invalid])
(def vnode-keys-text [:typeid :parent :node :text])

(deftype Parent [typeid])

(extend-protocol IPrintWithWriter
  Parent
  (-pr-writer [p writer opts]
    (-write writer "#parent[")
    (-write writer (if (.-typeid p) (str (.-typeid p)) "nil"))
    (-write writer "]")))

(defn format-vnode [vnode]
  (when vnode
    (let [vnode-keys (if (= -1 (aget vnode m/index-typeid)) vnode-keys-text vnode-keys)
          l (.-length vnode)]
      (loop [m (transient {})
             i 0]
        (if (< i l)
          (let [val (cond (= i m/index-parent-vnode)
                          (when-let [p (aget vnode m/index-parent-vnode)]
                            (Parent. (aget p m/index-typeid)))
                          (= i m/index-keymap)
                          (when-let [keymap (aget vnode m/index-keymap)]
                            (into #{} (o/getKeys keymap)))
                          :else (aget vnode i))]
            (recur (assoc! m (get vnode-keys i) val)
                   (inc i)))
          (persistent! m))))))

(declare format-tree)

(defn format-children [children]
  (when children
    (let [l (.-length children)]
      (loop [arr (transient [])
             i 0]
        (if (< i l)
          (recur (conj! arr (format-tree (aget children i)))
                 (inc i))
          (persistent! arr))))))

(defn format-tree [vnode]
  (let [vnode (format-vnode vnode)]
    (if (= -1 (aget vnode m/index-typeid))
      vnode
      (if (contains? vnode :children)
        (update-in vnode [:children] format-children)
        vnode))))

(defn root-vnode []
  (when-let [root (.getElementById js/document "root")]
    (format-tree (o/get root m/node-data-key))))
