(ns muance.comp-test
  (:require [cljs.pprint :refer [pprint pp]]
            [cljs.test :refer [deftest testing is run-tests]]
            [goog.object :as o]
            [muance.core :as m]
            [muance.dom :as dom]
            [muance.diff :as diff]
            [muance.print :as mprint]
            [muance.utils-test :as utils]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true])
  (:require-macros [muance.h :as h]))

(defonce vtree (atom nil))

(m/defcomp empty-comp-f [])

(deftest empy-comp []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree empty-comp-f))


(m/defcomp comp-props-inner-f [props]
  (h/div :id props))

(m/defcomp comp-props-f [props]
  (h/p :class props
       (comp-props-inner-f (+ 1 props))))

(deftest static-comp []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-props-f 53))




(m/defcomp comp-insert-before-inner []
  (h/b "rr")
  (h/p "inner"))

(m/defcomp comp-insert-before-f [bool]
  (h/div
   (if bool
     (comp-insert-before-inner)
     (do (h/a)
         (h/p "outer")))
   (h/p)))

(deftest insert-before []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-insert-before-f false))

(def keys-vec [[1 3 -2 -5 6 4]
               [3 1 -2 5 0 -6 7 4 8]
               [9 5 0]
               []
               [0 1 3 4 5]])

(m/defcomp comp-keyed-props [props]
  (h/p :class "props" (dom/text props))
  (h/div))

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
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-keyed-f {:keys (get keys-vec 2) :props "comp-props6"})
  )











(defn set-key-prop [type]
  (fn []
    (let [nodes (m/nodes)]
      (doseq [node nodes]
        (let [k (m/key)]
          (o/set node "keyedKey" k)
          (o/set node "keyedType" type))))))

(m/defcomp comp-keyed-generative-no-child
  ::m/hooks {:did-mount (set-key-prop 1)
             :did-update (set-key-prop 1)}
  [])

(m/defcomp comp-keyed-generative-child
  ::m/hooks {:did-mount (set-key-prop 2)
             :did-update (set-key-prop 2)}
  [k]
  (h/div :class k))

(m/defcomp comp-keyed-generative-children
  ::m/hooks {:did-mount (set-key-prop 3)
             :did-update (set-key-prop 3)}
  [k]
  (h/div :class k)
  (h/div :class k))

(m/defcomp comp-keyed-generative-f [keys-seq]
  (h/div :class ["reorder"]
         (doseq [[type k] keys-seq]
           (cond (= 0 type)
                 (if k
                   (h/div :class k
                          ::m/key k
                          ::m/hooks {:did-mount (set-key-prop 0)
                                     :did-update (set-key-prop 0)})
                   (h/div ::m/hooks {:did-mount (set-key-prop 0)
                                     :did-update (set-key-prop 0)}))
                 (= 1 type)
                 (if k
                   (comp-keyed-generative-no-child k)
                   (comp-keyed-generative-no-child))
                 (= 2 type)
                 (if k
                   (comp-keyed-generative-child k k)
                   (comp-keyed-generative-child nil))
                 (= 3 type)
                 (if k
                   (comp-keyed-generative-children k k)
                   (comp-keyed-generative-children nil))
                 (= 4 type)
                 (dom/text "ee")))))

(def keyed-generator
  (gen/list
   (gen/fmap
    (fn [keys-seq]
      (map (fn [k]
             (let [type (rand-int 5)]
               [type (when (and k (not (= 4 type))) (str k))]))
           keys-seq))
    (gen/set (gen/frequency [[10 (gen/choose 1 21)]
                             [1 (gen/return nil)]])))))

(defn keyed-expected [[t k :as x]]
  (cond (= 1 t) []
        (= 3 t) [x x]
        :else [x]))

(defn keyed-result [node]
  (if (= 3 (.-nodeType node))
    [4 nil]
    [(o/get node "keyedType") (o/get node "keyedKey")]))

(def comp-keyed-generative
  (prop/for-all [keyed-seq keyed-generator]
                (swap! vtree utils/new-vtree {:synchronous? true})
                (m/append-child (utils/new-root) @vtree)
                (loop [[keys-seq & rest-keyed] keyed-seq]
                  (if keys-seq
                    (let [expected (mapcat keyed-expected keys-seq)]
                      (m/patch @vtree comp-keyed-generative-f keys-seq)
                      (let [reorder-node (.querySelector js/document ".reorder")
                            nodes (array-seq (.-childNodes reorder-node) 0)
                            result (map keyed-result nodes)]
                        (if (= expected result)
                          (recur rest-keyed)
                          false)))
                    true))))

(comment
  (do
    (swap! vtree utils/new-vtree {:synchronous? true})
    (m/append-child (utils/new-root) @vtree)
    (m/patch @vtree comp-keyed-generative-f '([2 nil]))
    #_(m/patch @vtree comp-keyed-generative-f '())
    (let [reorder-node (.querySelector js/document ".reorder")
          nodes (array-seq (.-childNodes reorder-node) 0)
          result (map keyed-result nodes)]
      result))
  )

(comment
  (tc/quick-check 100 comp-keyed-generative)
  )






(def keys-vec2 [[1]
                [-1 1]
                [0]])

(m/defcomp comp-attributes-props
  ::m/hooks {:did-mount (fn [props state]
                          (prn "did-mount")
                          (prn :props props)
                          (prn :state state)
                          (prn :component-name (m/component-name))
                          (prn :nodes (m/nodes)))
             :will-update (fn [props state]
                            (prn "will-update")
                            (prn :props props)
                            (prn :state state))
             :did-update (fn [props state]
                           (prn "did-update")
                           (prn :props props)
                           (prn :state state))
             :will-unmount (fn [props state]
                             (prn "will-unmount")
                             (prn :props props)
                             (prn :state state)
                             (prn :component-name (m/component-name))
                             (prn :nodes (m/nodes)))
             :get-initial-state (fn [props]
                                  (prn "get-initial-state")
                                  (prn :props props)
                                  "initial-state")
             :will-receive-props (fn [prev-props props state-ref]
                                   (reset! state-ref "new-state")
                                   (prn "will-receive-props")
                                   (prn :prev-props prev-props)
                                   (prn :props props)
                                   (prn :state-ref state-ref))}
  [props]
  (h/p :class "props"
       ::m/hooks {:did-mount (fn [props state]
                               (prn "did-mount-inner")
                               (prn :props props)
                               (prn :state state)
                               (prn :component-name (m/component-name))
                               (prn :node (m/node)))
                  :will-update (fn [props state]
                                 (prn "will-update-inner")
                                 (prn :props props)
                                 (prn :state state))
                  :did-update (fn [props state]
                                (prn "did-update-inner")
                                (prn :props props)
                                (prn :state state))
                  :will-unmount (fn [props state]
                                  (prn "will-unmount-inner")
                                  (prn :props props)
                                  (prn :state state)
                                  (prn :component-name (m/component-name))
                                  (prn :node (m/node)))}
       (dom/text props)))

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
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-attributes-f {:keys (get keys-vec2 0) :props "comp-props2"})
  )



(defn render-queue-click [e state-ref]
  (swap! state-ref inc))

(m/defcomp render-queue-depth2 [props]
  (h/div :aria-state (m/state)
         :class (:depth-1-state props)
         :style {:width "300px" :height "300px" :border "1px solid green"}
         ::m/on [:click render-queue-click]
         (dom/text props)))

(m/defcomp render-queue-depth1
  ::m/hooks {:get-initial-state (fn [props] 0)
             :will-receive-props (fn [prev-props props state]
                                   (reset! state (:depth1 props)))
             :did-mount (fn [props state]
                          (m/set :interval-id (dom/set-interval #(do (prn "inc")
                                                                     (swap! % inc)) 1000)))
             :will-unmount (fn [props state]
                             (let [interval-id (m/get :interval-id)]
                               ;; throws an exception
                               #_(.clearInterval interval-id)
                               (.clearInterval js/window interval-id)))}
  [props]
  (h/p :class (m/state)
       :id props
       :style {:width "500px" :height "500px" :border "1px solid black"}
       ::m/on [:click render-queue-click]
       (render-queue-depth2 (assoc (select-keys props [:depth2]) :depth-1-state (m/state)))))

(m/defcomp render-queue-depth1* []
  (h/p :class (m/state)
       :style {:width "500px" :height "500px" :border "1px solid red"}
       ::m/on [:click render-queue-click]))

(m/defcomp render-queue-depth0 [props]
  (h/div (when (:display props) (render-queue-depth1 props))
         (render-queue-depth1* 1)))



(deftest render-queue []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree render-queue-depth0 {:depth1 42 :depth2 "depth2-props" :display false}))





(m/defcomp comp-svg-inner []
  (prn diff/*svg-namespace*)
  (h/rect :width "500px" :height "500px"
          ::m/on [:click (fn [e state-ref]
                           (swap! state-ref inc))]
          (h/foreignObject (h/p 
                            (dom/text "eerrgg")))))

(m/defcomp comp-svg-top []
  (h/svg :width "500px" :height "500px" (comp-svg-inner))
  (h/a))

(deftest comp-svg []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-svg-top))



(defn exception-click-handler [e state-ref]
  (swap! state-ref inc))

(m/defcomp comp-exception-keyed
  ::m/hooks {:will-update (fn [b]
                            (prn (m/key))
                            (when (= (m/key) "3")
                              #_(throw (js/Error. "err"))))
             :will-unmount (fn [b]
                             (when (= (m/key) "3")
                               #_(throw (js/Error. "err"))))}
  [b]
  (let [comp-k (m/key)]
    (h/p
     :style {:width "500px" :height "500px"}
     ::m/on [:click exception-click-handler]
     (dom/text comp-k " " (m/state)))))

#_(m/defcomp comp-exception-f [b]
    (if b
      (do (comp-exception-keyed 1 nil) (comp-exception-keyed 2 nil)
          (comp-exception-keyed 3 b))
      (do (comp-exception-keyed 1 nil) (comp-exception-keyed 4 nil)
          (comp-exception-keyed 3 b) (comp-exception-keyed 2 nil))))

(m/defcomp comp-exception-f [b]
  (if b
    (do (comp-exception-keyed 1 nil) (comp-exception-keyed 2 nil)
        (comp-exception-keyed 3 b) (comp-exception-keyed 4 nil))
    (do (comp-exception-keyed 1 nil) (comp-exception-keyed 4 nil)
        (h/p) (comp-exception-keyed 2 nil))))

(deftest comp-exception []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-exception-f false))



(m/defcomp comp-prevent-node-removal-f [b]
  (if b
    (h/p
     #_::m/hooks #_{:remove-hook (fn [rem-node] (dom/remove-node rem-node))}
     (h/div
      ::m/hooks {:remove-hook (fn [rem-node]
                                (dom/set-timeout (fn [state-ref] (dom/remove-node rem-node))
                                                 3000))}))
    (h/p)))

(deftest comp-prevent-node-removal []
  (swap! vtree utils/new-vtree)
  (m/append-child (utils/new-root) @vtree)
  (m/patch @vtree comp-prevent-node-removal-f true))
