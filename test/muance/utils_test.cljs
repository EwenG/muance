(ns muance.utils-test
  (:require [goog.dom :as dom]))

(defn new-root []
  (when-let [root (.getElementById js/document "root")]
    (dom/removeNode root))
  (let [root (doto (js/document.createElement "div")
               (aset "id" "root"))]
    (.appendChild js/document.body root)
    root))

(defn root []
  (.getElementById js/document "root"))
