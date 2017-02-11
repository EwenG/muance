(ns muance.core-test
  (:require [cljs.core :as c :refer-macros [str]]
            [muance.core :as m :include-macros]
            [goog.dom :as dom]
            [goog.object :as o])
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
  (m/defcomp f [x]
    (m/div :lifecycle #js {:didMount (fn [props] (prn "mount " props))
                           :willUpdate (fn [props] (prn "wupdate " props))
                           :willUnmount (fn [state] (prn "wunmount " state))}
           (if x
             (do (m/with-key 33 (m/p)))
             (do (m/p)
                 (m/with-key 33 (m/p :lifecycle
                                     #js {:didUpdate (fn [props]
                                                       (prn "dupdate2  " props (m/did-move?)))}))
                 (m/div :lifecycle #js {:willUnmount (fn [state]
                                                       (prn "wunmount2 " state))})))))
  (m/patch (.getElementById js/document "root") f false)

  (m/defcomp aa [e]
    (m/div (when e (m/attrs "ff" "ff")) (when e (m/attrs "class" e "dd" 33))))
  (m/patch (.getElementById js/document "root") aa false)

  (macroexpand-1 '(m/p (m/div)))
  (macroexpand-1 '(m/p (str "e") (m/div {:key 3 :lifecycle {:did-mount #(prn "e") :will-update (fn [])}})))
  (macroexpand-1 '(m/p (str "e") (m/div {:key 3 :lifecycle lifecycle-obj})))

  (let [e "e"] (m/p (m/p)))
  (m/p (str "e"))

  (macroexpand-1 '(m/p (m/attrs :e "e")))
  (macroexpand-1 '(m/p (m/attrs :e e)))

  (m/with-opts #js {:e "e"} 3)

  (m/defcomp cc "eee" [props] 3)
  
  )
