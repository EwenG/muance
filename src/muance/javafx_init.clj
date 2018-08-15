(ns muance.javafx-init
  (:import [javafx.application Application]))

(defn start-app []
  (when-not (realized? muance.javafx.MuanceFxApp/stagePromise)
    (let [app-thread (proxy [Thread] []
                       (run [] (Application/launch
                                muance.javafx.MuanceFxApp
                                (make-array String 0))))]
      (.start app-thread)
      @muance.javafx.MuanceFxApp/stagePromise)))

;; Some Classes cannot be loaded before the javax platform is initialized. It is initialized by start-app.


