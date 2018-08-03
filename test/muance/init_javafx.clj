(ns muance.init-javafx
  (:require [muance.utils-test :as utils]
            [muance.javafx :as javafx])
  (:import [javafx.application Application]
           [javafx.scene Group Scene]))

(defonce started-promise (promise))

(gen-class
 :name "javafx.test.MuanceFxApp"
 :extends javafx.application.Application)

(defn- -start [this s]
  (alter-var-root #'utils/stage (constantly s))
  (deliver started-promise s))

(defn- init-stage []
  (let [root (Group.)]
    (.setScene utils/stage (Scene. root 300 250))
    (.show utils/stage)))

(defn start-app []
  (when-not (realized? started-promise)
    (let [app-thread (proxy [Thread] []
                       (run [] (Application/launch
                                javafx.test.MuanceFxApp
                                (make-array String 0))))]
      (.start app-thread)
      @started-promise)))

(start-app)
(javafx/run-later (init-stage))

(comment
  (binding [*compile-path* "target/classes"]
    (compile 'muance.init-javafx))
  
  )
