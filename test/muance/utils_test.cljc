(ns muance.utils-test
  (:require #?(:cljs [goog.dom :as gdom])
            [muance.core :as m]
            [muance.print :as mprint]
            #?(:cljs [muance.dom :as dom])
            #?(:clj [muance.javafx :as javafx])))

(defonce v-state (atom nil))

(defn update-v-state [vtree]
  (reset! v-state (mprint/format-vtree vtree)))

#?(:cljs
   (defn new-root []
     (when-let [root (.getElementById js/document "root")]
       (gdom/removeNode root))
     (let [root (doto (js/document.createElement "div")
                  (aset "id" "root"))]
       (.appendChild js/document.body root)
       root)))

#?(:cljs
   (defn root []
     (.getElementById js/document "root")))

#?(:cljs
   (defn new-vtree
     ([vtree]
      (new-vtree vtree nil))
     ([vtree vtree-params]
      (when vtree
        (m/remove vtree))
      (dom/vtree (assoc vtree-params :post-render-hook update-v-state)))))

#?(:clj
   (defn new-vtree
     ([vtree]
      (new-vtree vtree nil))
     ([vtree vtree-params]
      (when vtree
        (javafx/run-later (m/remove vtree)))
      (javafx/vtree (assoc vtree-params :post-render-hook update-v-state)))))
