(ns muance.core-test
  (:require [cljs.core :as c :refer-macros [str]]
            [muance.core :as m]
            [goog.dom :as dom])
  #_(:refer-clojure :as c))

(comment

  (do
    (when-let [root (.getElementById js/document "root")]
      (dom/removeNode root))
    (.appendChild js/document.body (doto (js/document.createElement "div")
                                     (aset "id" "root"))))
  
  (m/patch (.-firstChild (.getElementById js/document "root"))
           (fn [] (m/p)))

  (macroexpand-1 '(m/div (m/div) (m/with-key 33 (m/p))))
  (defn f [x] (m/div :lifecycle #js {:didMount (fn [] (prn "mount"))}
                     (if x
                       (do (m/with-key 33 (m/p)))
                       (do (m/with-key 33 (m/p)) (m/div)))))
  (m/patch (.getElementById js/document "root") f false)

  (macroexpand-1 '(m/p (m/div)))
  (macroexpand-1 '(m/p (str "e") (m/div {:key 3 :lifecycle {:did-mount #(prn "e") :will-update (fn [])}})))
  (macroexpand-1 '(m/p (str "e") (m/div {:key 3 :lifecycle lifecycle-obj})))

  (let [e "e"] (m/p (m/p)))
  (m/p (str "e"))

  (macroexpand-1 '(m/p (m/attrs :e "e")))
  (macroexpand-1 '(m/p (m/attrs :e e)))

  (m/defcomp cc "eee" [props] 3)

  (m/with-opts #js {:e "e"} 3)
  
  )
