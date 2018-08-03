(ns muance.re-render-test
  (:require [muance.print :as mprint]
            [muance.core-test]
            [muance.comp-test]
            [muance.core :as m]
            [muance.diff :as diff]))

(m/re-render-on-update muance)

(defonce v-state (atom nil))

(defn post-render-always [vtree]
  (m/post-render
   vtree
   (fn post-render-hook []
     (reset! v-state (mprint/format-vtree vtree))
     (post-render-always vtree))))

(add-watch muance.core-test/vtree ::v-state
           (fn [k r o n]
             (post-render-always n)))

(add-watch muance.comp-test/vtree ::comp-v-state
           (fn [k r o n]
             (post-render-always n)))

(comment
  (mprint/format-vtree @muance.core-test/vtree)

  (m/post-render @muance.core-test/vtree
                 (fn post-render-hook []
                   (prn "33")
                   ))

  (.-render-queue @muance.core-test/vtree)

  (do (.-vnode @muance.core-test/vtree)
      nil)
  )
