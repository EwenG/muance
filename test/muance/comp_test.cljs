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
  (reset! vtree (m/vtree (utils/new-root)))
  (m/patch @vtree empty-comp-f))


(m/defcomp comp-props-inner-f [props]
  (h/div :id props))

(m/defcomp comp-props-f [props]
  (h/p :class props
       (comp-props-inner-f (+ 1 props))))

(deftest static-comp []
  (reset! vtree (m/vtree (utils/new-root)))
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
  (reset! vtree (m/vtree (utils/new-root)))
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
  (reset! vtree (m/vtree (utils/new-root)))
  (m/patch @vtree comp-keyed-f {:keys (get keys-vec 0) :props "comp-props3"})
  )




(def keys-vec2 [[1]
                [-1 1]])

(m/defcomp comp-attributes-props [props]
  (h/p :class "props"
       ::m/hooks {:did-mount (fn [props state-ref]
                              (prn "did-mount-inner")
                              (prn :props props)
                              (prn :state-ref state-ref)
                              (prn :component-name (m/component-name m/*vnode*))
                              (prn :node (m/dom-node m/*vnode*)))
                  :will-update (fn [props state]
                                (prn "will-update-inner")
                                (prn :props props)
                                (prn :state state)
                                (prn (m/moving? m/*vnode*)))
                  :did-update (fn [props state]
                               (prn "did-update-inner")
                               (prn :props props)
                               (prn :state state)
                               (prn (m/moving? m/*vnode*)))
                  :will-unmount (fn [props state]
                                 (prn "will-unmount-inner")
                                 (prn :props props)
                                 (prn :state state)
                                 (prn :component-name (m/component-name m/*vnode*))
                                 (prn :node (m/dom-node m/*vnode*))
                                 )}
       (m/text props)))

(m/hooks comp-attributes-props
         {:did-mount (fn [props state-ref]
                      (prn "did-mount")
                      (prn :props props)
                      (prn :state-ref state-ref)
                      (prn :component-name (m/component-name m/*vnode*))
                      (prn :nodes (m/dom-nodes m/*vnode*)))
          :will-update (fn [props state]
                        (prn "will-update")
                        (prn :props props)
                        (prn :state state)
                        (prn (m/moving? m/*vnode*)))
          :did-update (fn [props state]
                       (prn "did-update")
                       (prn :props props)
                       (prn :state state)
                       (prn (m/moving? m/*vnode*)))
          :will-unmount (fn [props state]
                         (prn "will-unmount")
                         (prn :props props)
                         (prn :state state)
                         (prn :component-name (m/component-name m/*vnode*))
                         (prn :nodes (m/dom-nodes m/*vnode*)))
          :get-initial-state (fn [props]
                             (prn "get-initial-state")
                             (prn :props props)
                             "initial-state")
          :will-receive-props (fn [prev-props props state-ref]
                              (reset! state-ref "new-state")
                              (prn "will-receive-props")
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
  (reset! vtree (m/vtree (utils/new-root)))
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
         {:get-initial-state (fn [props]
                             0)
          :will-receive-props (fn [prev-props props state]
                              (reset! state (:depth1 props)))
          :did-mount (fn [props state-ref]
                      (let [node (m/dom-node m/*vnode*)]
                        (swap! state-ref inc)
                        (o/set node (m/component-name m/*vnode*)
                               (.setInterval js/window #(swap! state-ref inc) 1000))))
          :will-unmount (fn [props state]
                         (let [node (m/dom-node m/*vnode*)
                               interval-id (o/get node (m/component-name m/*vnode*))]
                           #_(prn interval-id)
                           (.clearInterval js/window interval-id)))})

(deftest render-queue []
  (reset! vtree (m/vtree (utils/new-root)))
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
  (reset! vtree (m/vtree (utils/new-root)))
  (m/patch @vtree comp-svg-top))

(comment

  (cljs.pprint/pprint (utils/root-vnode @vtree))
  (cljs.pprint/pprint (utils/render-queue @vtree))
  
  )
