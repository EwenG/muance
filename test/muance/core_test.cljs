(ns muance.core-test
  (:require [cljs.pprint :refer [pprint pp]]
            [cljs.test :refer [deftest testing is run-tests]]
            [goog.dom :as dom]
            [goog.object :as o]
            [muance.core :as m :include-macros true]
            [muance.utils-test :as utils]
            [muance.custom-tags :as tag :include-macros true]))


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



(defn attrs-f [{:keys [class1 dyn-attr bg-cond color
                       input-value
                       for-val
                       checkbox-value checkbox-checked
                       selected]}]
  (m/div :class [class1 "f"]
         :foo nil
         :static-attr "static-attr"
         :dyn-attr dyn-attr
         :styles {:background (when bg-cond "black") :color color})
  (m/input :type "text" :value input-value)
  (m/label :for for-val)
  (m/input :type "checkbox" :value checkbox-value :checked checkbox-checked)
  (m/input :type "file" :name "rr" :multiple true)
  (m/select
   (m/option :value "val1")
   (m/option :value "val2" :selected selected)))

(deftest attrs []
  (utils/new-root)

  (m/patch (utils/root) attrs-f
           {:class1 88 :dyn-attr "val" :bg-cond true :color "green"
            :input-value "tt4"
            :for-val "rr2"
            :checkbox-value nil :checkbox-checked "e"
            :selected false})
  )



(defn text-f [x]
  (m/div (if x (m/text "e") (m/p)) "<p></p>"))

(deftest text []
  (utils/new-root)

  (m/patch (utils/root) text-f false))



(defn custom-tag-f []
  (tag/custom-tag))

(deftest custom-tag []
  (utils/new-root)
  
  (m/patch (utils/root) custom-tag-f))




(defn svg-f [href]
  (m/svg :xml:lang "fr"
   (m/svg
    (m/a :xlink:href href)
    (when true (m/altGlyph))
    (m/foreignObject
     (m/svg)
     (m/div)))
   (m/font-face-name))
  (m/p))

(deftest svg []
  (utils/new-root)
  
  (m/patch (utils/root) svg-f "rr"))




(defn custom-css-f [bg]
  (m/div :styles {:--background bg}))

(deftest custom-css []
  (utils/new-root)
  
  (m/patch (utils/root) custom-css-f "red"))


(defn click-handler [e state attr1 attr2 attr3 attr4]
  (prn "clicked")
  (prn state)
  (prn attr1)
  (prn attr2)
  (prn attr3))

(defn mouseover-handler [e state]
  (prn "mouseover2")
  (prn state))

(defn handlers-f [[w handler c]]
  (m/div
         :class c
         :on [[:click handler "attr1" 2 "attr3"] [:mouseover mouseover-handler]]
         :styles {:width w :height "500px"}))

(deftest handlers []
  (utils/new-root)
  
  (m/patch (utils/root) handlers-f ["503px" click-handler "class4"]))


(comment

  (cljs.pprint/pprint (utils/root-vnode))

  )
