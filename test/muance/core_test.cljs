(ns muance.core-test
  (:require [cljs.pprint :refer [pprint pp]]
            [cljs.test :refer [deftest testing is run-tests]]
            [goog.dom :as dom]
            [goog.object :as o]
            [muance.core :as m :include-macros]
            [muance.utils-test :as utils]))


(defn root-static-f []
  (m/div) (m/p))

(deftest root-static []
  (utils/new-root)
  (m/patch (utils/root) root-static-f))



(defn static-f []
  (m/div (m/div) (m/p) (m/div)))

(deftest static []
  (utils/new-root)
  (m/patch (utils/root) static-f))



(def nodes-vec [[:p :div :p :p]
                [:div :p]
                []
                [:div :div :p :div :div :p]])

(defn root-nodes-f [nodes]
  (doseq [n nodes]
    (case n
      :p (m/p)
      :div (m/div))))

(deftest root-nodes []
  (utils/new-root)
  (m/patch (utils/root) root-nodes-f (get nodes-vec 0))
  (m/patch (utils/root) root-nodes-f (get nodes-vec 1))
  (m/patch (utils/root) root-nodes-f (get nodes-vec 2))
  (m/patch (utils/root) root-nodes-f (get nodes-vec 3)))



(defn nodes-f [nodes]
  (m/div
   (doseq [n nodes]
     (case n
       :p (m/p)
       :div (m/div)))))

(deftest nodes []
  (utils/new-root)
  (m/patch (utils/root) nodes-f (get nodes-vec 0))
  (m/patch (utils/root) nodes-f (get nodes-vec 1))
  (m/patch (utils/root) nodes-f (get nodes-vec 2))
  (m/patch (utils/root) nodes-f (get nodes-vec 3)))



(def keys-vec [[1 2 3 4 5 6]
               [1 3 2 5 6 4]
               [3 1 2 5 -1 6 7 4 8]
               [9 5 -1]
               []
               [-1 1 3 4 5]])

(defn keyed-f [keys]
  (m/div
   (doseq [k keys]
     (if (= -1 k)
       (m/p)
       (m/p :key k)))))

(deftest keyed []
  (utils/new-root)
  
  (m/patch (utils/root) keyed-f (get keys-vec 0))
  (m/patch (utils/root) keyed-f (get keys-vec 1))
  (m/patch (utils/root) keyed-f (get keys-vec 2))
  (m/patch (utils/root) keyed-f (get keys-vec 3))
  (m/patch (utils/root) keyed-f (get keys-vec 4))
  (m/patch (utils/root) keyed-f (get keys-vec 5)))



(deftest duplicate-key []
  (utils/new-root)
  
  (m/patch (utils/root) keyed-f [1])
  (m/patch (utils/root) keyed-f [1 1])
  (m/patch (utils/root) keyed-f [2 3 1 1 1]))



(defn mismatch-key-typeid-f [x]
  (m/div
   (if x
     (do (m/div :key 1) (m/p :key 2))
     (do (m/div :key 2) (m/p :key 1)))))

(deftest mismatch-key-typeid []
  (utils/new-root)
  
  (m/patch (utils/root) mismatch-key-typeid-f true)
  (m/patch (utils/root) mismatch-key-typeid-f false))



(defn attrs-f [props]
  (m/div :class [(get props 0) 33]
         :ff (when false "e")
         :gg "t"
         :styles {:background (when (get props 1) "black") :color (get props 1)}))

(deftest attrs []
  (utils/new-root)

  (m/patch (utils/root) attrs-f [88 "red"])
  )


(comment

  (cljs.pprint/pprint (utils/root-vnode))
  
  )
