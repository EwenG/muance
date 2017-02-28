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




(m/defcomp comp-insert-before-inner []
  #_(h/b "rr"))

(m/defcomp comp-insert-before-f [bool]
  (h/div
   (if bool
     (comp-insert-before-inner)
     (h/a))
   (h/p)))

(deftest insert-before []
  (utils/new-root)
  (m/patch (utils/root) comp-insert-before-f false))



(def keys-vec [[1 3 -2 -5 6 4]
               [3 1 -2 5 0 -6 7 4 8]
               [9 5 0]
               []
               [0 1 3 4 5]])

(m/defcomp comp-keyed-props [props]
  (h/p :class "props" (m/text props)))

(m/defcomp comp-keyed-no-props []
  (h/p :class "no-props"))

(m/defcomp comp-keyed-f [{:keys [keys props]}]
  (h/div
   (doseq [k keys]
     (cond (= 0 k)
           (comp-keyed-no-props)
           (< k 0)
           (comp-keyed-no-props k)
           :else
           (comp-keyed-props k props)))))

(deftest comp-keyed []
  (utils/new-root)
  (m/patch (utils/root) comp-keyed-f {:keys (get keys-vec 0) :props "comp-props3"})
  )




(def keys-vec2 [[1]
                [-1 1]])

(m/defcomp comp-attributes-props [props]
  (h/p :class "props"
       ::m/hooks {:didMount (fn [props state]
                              (prn "didMountInner")
                              (prn :props props)
                              (prn :state state)
                              (prn m/*component-name*))
                  :willUpdate (fn [props state]
                                (prn "willUpdateInner")
                                (prn :props props)
                                (prn :state state)
                                (prn m/*moving*))
                  :didUpdate (fn [props state]
                               (prn "didUpdateInner")
                               (prn :props props)
                               (prn :state state)
                               (prn m/*moving*))
                  :willUnmount (fn [state]
                                 (prn "willUnmountInner")
                                 (prn :state state)
                                 (prn m/*component-name*))}
       (m/text props)))

(m/hooks comp-attributes-props
         {:didMount (fn [props state]
                      (prn "didMount")
                      (prn :props props)
                      (prn :state state))
          :willUpdate (fn [props state]
                        (prn "willUpdate")
                        (prn :props props)
                        (prn :state state)
                        (prn m/*moving*))
          :didUpdate (fn [props state]
                       (prn "didUpdate")
                       (prn :props props)
                       (prn :state state)
                       (prn m/*moving*))
          :willUnmount (fn [state]
                         (prn "willUnmount")
                         (prn :state state))
          :getInitialState (fn [props]
                             (prn "getInitialState")
                             (prn :props props)
                             "initialState")
          :willReceiveProps (fn [props]
                              (prn "willReceiveProps")
                              (prn :props props)
                              "newState")})

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
  (m/patch (utils/root) comp-attributes-f {:keys (get keys-vec2 0) :props "comp-props3"})
  )

(comment

  (cljs.pprint/pprint (utils/root-vnode))

  )
