(ns muance.comp-test
  (:require [cljs.pprint :refer [pprint pp]]
            [cljs.test :refer [deftest testing is run-tests]]
            [goog.dom :as dom]
            [goog.object :as o]
            [muance.core :as m :include-macros true]
            [muance.utils-test :as utils])
  (:require-macros [muance.h :as h]))

(defonce vtree (atom nil))

(m/defcomp empty-comp-f [])

(deftest empy-comp []
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree empty-comp-f))


(m/defcomp comp-props-inner-f [props]
  (h/div :id props))

(m/defcomp comp-props-f [props]
  (h/p :class props
       (comp-props-inner-f (+ 1 props))))

(deftest static-comp []
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree comp-props-f 48))




(m/defcomp comp-insert-before-inner []
  (h/b "rr"))

(m/defcomp comp-insert-before-f [bool]
  (h/div
   (if bool
     (comp-insert-before-inner)
     (h/a))
   (h/p)))

(deftest insert-before []
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree comp-insert-before-f false))



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
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree comp-keyed-f {:keys (get keys-vec 0) :props "comp-props3"})
  )




(def keys-vec2 [[1]
                [-1 1]])

(m/defcomp comp-attributes-props [props]
  (h/p :class "props"
       ::m/hooks {:didMount (fn [props state-ref]
                              (prn "didMountInner")
                              (prn :props props)
                              (prn :state-ref state-ref)
                              (prn :component-name (m/component-name m/*current-vnode*))
                              (prn :node (m/dom-node m/*current-vnode*)))
                  :willUpdate (fn [props state]
                                (prn "willUpdateInner")
                                (prn :props props)
                                (prn :state state)
                                (prn (m/moving? m/*current-vnode*)))
                  :didUpdate (fn [props state]
                               (prn "didUpdateInner")
                               (prn :props props)
                               (prn :state state)
                               (prn (m/moving? m/*current-vnode*)))
                  :willUnmount (fn [props state]
                                 (prn "willUnmountInner")
                                 (prn :props props)
                                 (prn :state state)
                                 (prn :component-name (m/component-name m/*current-vnode*))
                                 (prn :node (m/dom-node m/*current-vnode*))
                                 )}
       (m/text props)))

(m/hooks comp-attributes-props
         {:didMount (fn [props state-ref]
                      (prn "didMount")
                      (prn :props props)
                      (prn :state-ref state-ref)
                      (prn :component-name (m/component-name m/*current-vnode*))
                      (prn :nodes (m/dom-nodes m/*current-vnode*)))
          :willUpdate (fn [props state]
                        (prn "willUpdate")
                        (prn :props props)
                        (prn :state state)
                        (prn (m/moving? m/*current-vnode*)))
          :didUpdate (fn [props state]
                       (prn "didUpdate")
                       (prn :props props)
                       (prn :state state)
                       (prn (m/moving? m/*current-vnode*)))
          :willUnmount (fn [props state]
                         (prn "willUnmount")
                         (prn :props props)
                         (prn :state state)
                         (prn :component-name (m/component-name m/*current-vnode*))
                         (prn :nodes (m/dom-nodes m/*current-vnode*)))
          :getInitialState (fn [props]
                             (prn "getInitialState")
                             (prn :props props)
                             "initialState")
          :willReceiveProps (fn [prev-props props state-ref]
                              (reset! state-ref "newState")
                              (prn "willReceiveProps")
                              (prn :prev-props prev-props)
                              (prn :props props)
                              (prn :state-ref state-ref))})

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
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree comp-attributes-f {:keys (get keys-vec2 0) :props "comp-props1"})
  )



(defn render-queue-click [e state-ref]
  (swap! state-ref inc))

(m/defcomp render-queue-depth2 [props]
  (h/div :aria-state m/*state*
         :class (:depth1-state props)
         :style {:width "300px" :height "300px" :border "1px solid green"}
         ::m/on [:click render-queue-click]
         (m/text props)))

(m/defcomp render-queue-depth1 [props]
  (h/p :class m/*state*
       :id props
       :style {:width "500px" :height "500px" :border "1px solid black"}
       ::m/on [:click render-queue-click]
       (render-queue-depth2 (assoc (select-keys props [:depth2]) :depth-1-state m/*state*))))

(m/defcomp render-queue-depth1* []
  (h/p :class m/*state*
       :style {:width "500px" :height "500px" :border "1px solid red"}
       ::m/on [:click render-queue-click]))

(m/defcomp render-queue-depth0 [props]
  (h/div (when (:display props) (render-queue-depth1 props))
         (render-queue-depth1*)))

(m/hooks render-queue-depth1
         {:getInitialState (fn [props]
                             0)
          :willReceiveProps (fn [prev-props props state]
                              (reset! state (:depth1 props)))
          :didMount (fn [props state-ref]
                      (let [node (m/dom-node m/*current-vnode*)]
                        (swap! state-ref inc)
                        (o/set node (m/component-name m/*current-vnode*)
                               (.setInterval js/window #(swap! state-ref inc) 1000))))
          :willUnmount (fn [props state]
                         (let [node (m/dom-node m/*current-vnode*)
                               interval-id (o/get node (m/component-name m/*current-vnode*))]
                           #_(prn interval-id)
                           (.clearInterval js/window interval-id)))})

(deftest render-queue []
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree render-queue-depth0 {:depth1 42 :depth2 "depth4-props" :display false}))





(m/defcomp comp-svg-inner []
  #_(prn m/*svg-namespace*)
  (h/rect :width "500px" :height "500px"
          ::m/on [:click (fn [e state-ref]
                           (swap! state-ref inc))]
          (h/foreignObject (h/p 
                            (m/text "eerrgg")))))

(m/defcomp comp-svg-top []
  (h/svg :width "500px" :height "500px" (comp-svg-inner))
  (h/a))

(deftest comp-svg []
  (reset! vtree (m/vtree-init (utils/new-root)))
  (m/patch @vtree comp-svg-top))

(comment

  (cljs.pprint/pprint (utils/root-vnode @vtree))
  (cljs.pprint/pprint (utils/render-queue @vtree))
  
  )
