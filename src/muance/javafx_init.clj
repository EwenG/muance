(ns muance.javafx-init
  (:import [javafx.application Application]
           [javafx.scene Group Scene]))

(defonce stage-promise (promise))

(gen-class
 :name "javafx.test.MuanceFxApp"
 :extends javafx.application.Application)

(defn- -start [this s]
  (deliver stage-promise s))

(defn start-app []
  (when-not (realized? stage-promise)
    (let [app-thread (proxy [Thread] []
                       (run [] (Application/launch
                                javafx.test.MuanceFxApp
                                (make-array String 0))))]
      (.start app-thread)
      @stage-promise)))

;; Some Classes cannot be loaded before the javax platform is initialized. It is initialized by start-app.
