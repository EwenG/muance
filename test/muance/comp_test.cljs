(ns muance.comp-test
  (:require [cljs.pprint :refer [pprint pp]]
            [cljs.test :refer [deftest testing is run-tests]]
            [goog.dom :as dom]
            [goog.object :as o]
            [muance.core :as m :include-macros true]
            [muance.utils-test :as utils])
  (:require-macros [muance.h :as h]))

(m/defcomp empty-comp-f [])

(deftest empy-comp []
  (utils/new-root)
  (m/patch (utils/root) empty-comp-f))


(m/defcomp comp-props-inner-f [props]
  (h/div :id props))

(m/defcomp comp-props-f [props]
  (h/p :class props
       (comp-props-inner-f (+ 1 props))))

(deftest static-comp []
  (utils/new-root)
  (m/patch (utils/root) comp-props-f 47))



(def keys-vec [[1 -2 3 4 -5 6]
               [1 3 -2 -5 6 4]
               [3 1 -2 5 0 -6 7 4 8]
               [9 5 0]
               []
               [0 1 3 4 5]])

(m/defcomp comp-attributes-props [props]
  (h/p :class "props" (m/text props)))

(m/defcomp comp-attributes-no-props []
  (h/p :class "no-props"))

(m/defcomp comp-attributes-f [{:keys [keys props]}]
  (h/div
   (doseq [k keys]
     (cond (= 0 k)
           (comp-attributes-no-props)
           (< k 0)
           (comp-attributes-no-props k)
           :else
           (comp-attributes-props k props)))))

(deftest comp-attributes []
  (utils/new-root)
  (m/patch (utils/root) comp-attributes-f {:keys (get keys-vec 6) :props "comp-props3"}))

(comment

  (cljs.pprint/pprint (utils/root-vnode))

  )
