(ns muance.comp-test
  (:require [cljs.pprint :refer [pprint pp]]
            [cljs.test :refer [deftest testing is run-tests]]
            [goog.dom :as dom]
            [goog.object :as o]
            [muance.core :as m :include-macros true]
            [muance.utils-test :as utils])
  (:require-macros [muance.h :as h]))

(defonce root (atom nil))

(m/defcomp empty-comp-f [])

(deftest empy-comp []
  (reset! root (m/init-vtree (utils/new-root)))
  (m/patch-root @root empty-comp-f))


(m/defcomp comp-props-inner-f [props]
  (h/div :id props))

(m/defcomp comp-props-f [props]
  (h/p :class props
       (comp-props-inner-f (+ 1 props))))

(deftest static-comp []
  (reset! root (m/init-vtree (utils/new-root)))
  (m/patch-root @root comp-props-f 47))




(m/defcomp comp-insert-before-inner []
  #_(h/b "rr"))

(m/defcomp comp-insert-before-f [bool]
  (h/div
   (if bool
     (comp-insert-before-inner)
     (h/a))
   (h/p)))

(deftest insert-before []
  (reset! root (m/init-vtree (utils/new-root)))
  (m/patch-root @root comp-insert-before-f false))



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
  (reset! root (m/init-vtree (utils/new-root)))
  (m/patch-root @root comp-keyed-f {:keys (get keys-vec 0) :props "comp-props3"})
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
  (reset! root (m/init-vtree (utils/new-root)))
  (m/patch-root @root comp-attributes-f {:keys (get keys-vec2 0) :props "comp-props3"})
  )




(m/defcomp render-queue-depth2 [props]
  )

(defn render-queue-click [e state-ref]
  (swap! state-ref inc))

(m/defcomp render-queue-depth1 [props]
  (h/p :class m/*state*
       :styles {:width "500px" :height "500px" :border "1px solid black"}
       ::m/on [:click render-queue-click]
       (render-queue-depth2 (:depth2 props))))

(m/defcomp render-queue-depth1* []
  (h/p :class m/*state*
       :styles {:width "500px" :height "500px" :border "1px solid red"}
       ::m/on [:click render-queue-click]))

(m/defcomp render-queue-depth0 [props]
  (h/div (render-queue-depth1 props)
         (render-queue-depth1*)))

(m/hooks render-queue-depth1
         {:getInitialState (fn [props]
                             0)
          :willReceiveProps (fn [prev-props props]
                              (:depth1 props))})

(deftest render-queue []
  (reset! root (m/init-vtree (utils/new-root)))
  (m/patch-root @root render-queue-depth0 {:depth1 45 :depth2 "depth2-props"}))

(comment

  (m/patch-root @root cc)

  (cljs.pprint/pprint (utils/root-vnode @root))
  (cljs.pprint/pprint (utils/render-queue @root))
  
  )
