(ns muance.core-test
  (:require [cljs.test :refer [deftest testing is run-tests]]
            [goog.object :as o]
            [muance.core :as m :include-macros true]
            [muance.dom :as dom :include-macros true]
            [muance.attribute :as a]
            [muance.utils-test :as utils]
            [muance.custom-tags :as tag :include-macros true])
  (:require-macros [muance.h :as h]))

(defonce vtree (atom nil))

(defn root-static-f []
  (h/div) (h/p))

(m/defcomp root-static-c []
  (root-static-f))

(deftest root-static []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree root-static-c))



(defn static-f []
  (h/div (h/div) (h/p) (h/div)))

(m/defcomp static-c []
  (static-f))

(deftest static []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree static-c))



(def nodes-vec [[:p :div :p :p]
                [:div :p]
                []
                [:div :div :p :div :div :p]])

(defn root-nodes-f [nodes]
  (doseq [n nodes]
    (case n
      :p (h/p)
      :div (h/div))))

(m/defcomp root-nodes-c [nodes]
  (root-nodes-f nodes))

(deftest root-nodes []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree root-nodes-c (get nodes-vec 0))
  (m/patch @vtree root-nodes-c (get nodes-vec 1))
  (m/patch @vtree root-nodes-c (get nodes-vec 2))
  (m/patch @vtree root-nodes-c (get nodes-vec 3)))




(def keys-vec [[1 2 3 4 5 6]
               [1 3 2 5 6 4]
               [3 1 2 5 0 6 7 4 8]
               [9 5 0]
               []
               [0 1 3 4 5]])

(defn keyed-f [keys]
  (h/div
   (doseq [k keys]
     (if (= 0 k)
       (h/p)
       (h/p ::m/key k)))))

(m/defcomp keyed-c [keys]
  (keyed-f keys))

(deftest keyed []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree keyed-c (get keys-vec 0))
  (m/patch @vtree keyed-c (get keys-vec 1))
  (m/patch @vtree keyed-c (get keys-vec 2))
  (m/patch @vtree keyed-c (get keys-vec 3))
  (m/patch @vtree keyed-c (get keys-vec 4))
  (m/patch @vtree keyed-c (get keys-vec 5)))



(deftest duplicate-key []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree keyed-c [1])
  (m/patch @vtree keyed-c [1 1])
  (m/patch @vtree keyed-c [2 3 1 1 1]))


(defn mismatch-key-typeid-f [x]
  (h/div
   (if x
     (do (h/div ::m/key 1) (h/p ::m/key 2))
     (do (h/div ::m/key 2) (h/p ::m/key 1)))))

(m/defcomp mismatch-key-typeid-c [x]
  (mismatch-key-typeid-f x))

(deftest mismatch-key-typeid []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree mismatch-key-typeid-c true)
  (m/patch @vtree mismatch-key-typeid-c false))

(defn match-key-typeid-f [x]
  (let [x1 #(h/div ::m/key 1)
        x2 #(h/p ::m/key 2)]
    (h/div
     (if x
       (do (x1) (x2))
       (do (x2) (x1))))))

(m/defcomp match-key-typeid-c [x]
  (match-key-typeid-f x))

(deftest match-key-typeid []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree match-key-typeid-c true)
  (m/patch @vtree match-key-typeid-c false))



(defn attrs-f [{:keys [class1 dyn-attr bg-cond color
                       input-value
                       for-val
                       checkbox-value checkbox-checked
                       selected]}]
  (h/div :class [class1 "f"]
         :foo nil
         :data-static-attr "static-attr"
         :aria-dyn-attr dyn-attr
         :style {:background (when bg-cond "black") :color color})
  (h/input :type "text" :value input-value)
  (h/label :for for-val ::a/g "attribute-value")
  (h/input :type "checkbox" :value checkbox-value :checked checkbox-checked)
  (h/input :type "file" :name "rr" :multiple true)
  (h/select
   (h/option :value "val1" "rr")
   (h/option :value "val2" :selected selected)))

(m/defcomp attrs-c [x]
  (attrs-f x))

(deftest attrs []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree attrs-c
                {:class1 68 :dyn-attr "val4" :bg-cond true :color "green"
                 :input-value "tt8"
                 :for-val "rr1"
                 :checkbox-value nil :checkbox-checked "e"
                 :selected true}))



(defn text-f [x]
  (h/div (if x (dom/text "e") (h/p)) "<p></p>"))

(m/defcomp text-c [x]
  (text-f x))

(deftest text []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree text-c true))



(defn custom-tag-f []
  (tag/custom-tag))

(m/defcomp custom-tag-c []
  (custom-tag-f))

(deftest custom-tag []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree custom-tag-c))




(defn svg-f [href]
  (h/svg :xml:lang "fr" :svg-attr "svg attr"
   (h/svg
    (h/a :xlink:href href)
    (when true (h/altGlyph))
    (h/foreignObject
     (h/svg)
     (h/div)))
   (h/font-face-name))
  (h/p))

(m/defcomp svg-c [href]
  (svg-f href))

(deftest svg []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree svg-c "rr4"))




(defn custom-css-f [bg]
  (h/div :style {:--background bg}))

(m/defcomp custom-css-c [bg]
  (custom-css-f bg))

(deftest custom-css []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree custom-css-c "red"))


(defn click-handler [e state-ref attr1 attr2 attr3 attr4]
  (prn "clicked")
  (prn state-ref)
  (prn attr1)
  (prn attr2)
  (prn attr3))

(defn mouseover-handler [e state-ref]
  (prn "mouseover2")
  (prn state-ref))

(defn handlers-f [[w handler c a]]
  (h/div
   :class c
   ::m/on [[:click handler "attr1" 3 a 4] [:mouseover mouseover-handler]]
   :style {:width w :height "500px"}))

(m/defcomp handlers-c [x]
  (handlers-f x))

(deftest handlers []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree handlers-c ["503px" click-handler "class5" "a5"]))

;; Exceptions in tests are caught and *e is not set

