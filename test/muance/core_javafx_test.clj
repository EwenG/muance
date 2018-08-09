(ns muance.core-javafx-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [muance.utils-test :as utils]
            [muance.core :as m]
            [muance.javafx :as javafx]
            ;; muance.j must be required AFTER muance.init-javafx because muance.init-javafx initializes the javafx platform and muance.j uses javafx classes that require the platform to be initialized
            [muance.j :as j]))

(defn- init-stage []
  (let [root (javafx.scene.Group.)]
    (.setScene javafx/stage (javafx.scene.Scene. root 300 250))
    (.show javafx/stage)))

(defn scene []
  (-> javafx/stage (.getScene)))

(defn root []
  (-> (scene) (.getRoot)))

(defonce vtree (atom nil))

(defn handler [e state-ref]
  (prn (.getTarget e)))

(defn focus-handler [o n state-ref]
  (prn o n state-ref))

#_(defn root-static-f [{:keys [b f]}]
  (j/group
   (when b
     (j/textField ::m/on [:action f]
                  ::m/listen [:focused
                              (let [node (muance.arrays/aget
                                          muance.diff/*vnode*
                                          muance.diff/index-node)]
                                #_(prn node)
                                (fn [o n state-ref]
                                  (prn node)
                                  (prn o n state-ref)))]
                  :text "e"))))

(defn root-static-f [{:keys [b f]}]
  (j/group
   (when b
     (j/textField ::m/on [:action f]
                  ::m/listen [:focused
                              (fn [o n state-ref]
                                (prn o n state-ref))]
                  :text "e"))))

(m/defcomp root-static-c [p]
  (root-static-f p))

(deftest root-static []
  (swap! vtree utils/new-vtree true)
  (m/append-child (scene) @vtree)
  (m/patch @vtree root-static-c {:b true :f handler}))


(comment

  (javafx/run-later (init-stage))
  
  (root-static)

  (.getChildren (root))

  (. ^javafx.scene.control.TextField (javafx.scene.control.TextField.) focusedProperty)

  (loop [i 0
         b true]
    (when (< i 11)

      (m/patch @vtree root-static-c {:b b :f handler})
      (recur (inc i) (not b))))

  (loop [i 0
         b true]
    (when (< i 10)
      (javafx/run-later (prn 33))
      (recur (inc i) (not b))))
  )
