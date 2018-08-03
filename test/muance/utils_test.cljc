(ns muance.utils-test
  (:require #?(:cljs [goog.dom :as gdom])
            [muance.core :as m]
            #?(:cljs [muance.dom :as dom])
            #?(:clj [muance.javafx :as javafx])))

#?(:clj
   (defonce stage nil))

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
      (new-vtree vtree false))
     ([vtree async]
      (when vtree
        (m/remove vtree))
      (dom/vtree {:async async}))))

#?(:clj
   (defn new-vtree [vtree]
     (when vtree
       (m/remove vtree))
     (javafx/vtree)))
