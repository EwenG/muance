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


(m/defcomp static-comp-inner-f [props]
  (h/div :id props))

(m/defcomp static-comp-f [props]
  (h/p :class props
       (static-comp-inner-f (+ 1 props))))

(deftest static-comp []
  (utils/new-root)
  (m/patch (utils/root) static-comp-f 47))


(m/defcomp comp-attributes-f [])

#_( comp-attributes-f [])

(comment

  (cljs.pprint/pprint (utils/root-vnode))

  )
