(ns muance.core-javafx-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [muance.core :as m]
            [muance.utils-test :as utils]
            [muance.init-javafx :as init]
            [muance.javafx :as javafx]
            ;; muance.j must be required AFTER muance.init-javafx because muance.init-javafx initializes the javafx platform and muance.j uses javafx classes that require the platform to be initialized
            [muance.j :as j]))

(defn scene []
  (-> utils/stage (.getScene)))

(defn root []
  (-> (scene) (.getRoot)))

(defonce vtree (atom nil))

(defn root-static-f [x]
  (j/group
   ))

(m/defcomp root-static-c [x]
  (root-static-f x))

(deftest root-static []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree root-static-c false))


(comment
  (root-static)

  (.getChildren (root))
  )
