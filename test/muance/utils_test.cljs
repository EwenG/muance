(ns muance.utils-test
  (:require [goog.dom :as gdom]
            [muance.core :as m]
            [muance.print :as mprint]
            [muance.dom :as dom]))

(defonce v-state (atom nil))

(defn update-v-state [vtree]
  (reset! v-state (mprint/format-vtree vtree)))

(defn new-root []
  (when-let [root (.getElementById js/document "root")]
    (gdom/removeNode root))
  (let [root (doto (js/document.createElement "div")
               (aset "id" "root"))]
    (.appendChild js/document.body root)
    root))

(defn root []
  (.getElementById js/document "root"))

(defn new-vtree
  ([vtree]
   (new-vtree vtree nil))
  ([vtree vtree-params]
   (when vtree
     (m/remove vtree))
   (dom/vtree (assoc vtree-params :post-render-hook update-v-state))))

