(ns muance.javafx-init
  (:import [javafx.application Application]))

(defn- lauch-app []
  (Application/launch muance.javafx.MuanceFxApp (make-array String 0)))

(defn start-app []
  (when-not (realized? muance.javafx.MuanceFxApp/stagePromise)
    (doto (Thread. ^Runnable lauch-app)
      (.setDaemon false)
      (.start))
    @muance.javafx.MuanceFxApp/stagePromise))

;; Some Classes cannot be loaded before the javax platform is initialized. It is initialized by start-app.


