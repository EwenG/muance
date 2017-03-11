(ns muance.dbmonster
  (:require
   [muance.core :as m :include-macros true])
  (:require-macros [muance.h :as h]))

(m/defcomp dbmon [databases]
  (h/div
   (h/table
    :class ["table" "table-striped" "latest-data"]
    (h/tbody
     (doseq [database databases]
       (h/tr ::m/key (.-dbname database)
        (h/td :class "dbname"
              (m/text (.-dbname database)))
        (h/td
         :class "query-count"
         (h/span :class (.-countClassName (.-lastSample database))
                 (m/text (.-nbQueries (.-lastSample database)))))
        (doseq [query (.-topFiveQueries (.-lastSample database))]
          (h/td :class ["Query" (.-elapsedClassName query)]
                (m/text (.-formatElapsed query))
                (h/div :class ["popover left"]
                       (h/div :class "popover-content" (m/text (.-query query)))
                       (h/div :class "arrow"))))))))))

(defonce vtree (m/vtree false))
(m/remove vtree)
(m/append-child vtree (.-body js/document))

(defn load-samples []
  (m/patch vtree dbmon (-> js/ENV (.generateData) (.toArray)))
  (.ping (.-renderRate js/Monitoring))
  (.setTimeout js/window load-samples (.-timeout js/ENV)))


(load-samples)

(comment

  (m/defcomp css []
    (h/link :rel "stylesheet" :type "text/css" :href "styles.css"))
  (m/defcomp env []
    (h/script :src "ENV.js"))
  (m/defcomp memory-stats []
    (h/script :src "memory-stats.js"))
  (m/defcomp monitor []
    (h/script :src "monitor.js"))

  (defonce css-vtree (m/vtree))
  (m/remove css-vtree)
  (m/append-child css-vtree (.-body js/document))
  (m/patch css-vtree css)

  (defonce env-vtree (m/vtree))
  (m/remove env-vtree)
  (m/append-child env-vtree (.-body js/document))
  (m/patch env-vtree env)

  (defonce memory-stats-vtree (m/vtree))
  (m/remove memory-stats-vtree)
  (m/append-child memory-stats-vtree (.-body js/document))
  (m/patch memory-stats-vtree memory-stats)

  (defonce monitor-vtree (m/vtree))
  (m/remove monitor-vtree)
  (m/append-child monitor-vtree (.-body js/document))
  (m/patch monitor-vtree monitor)
  
  (defn loadSamples []
    (m/patch vtree dbmon (-> js/ENV (.generateData) (.toArray)))
    (.setTimeout js/window loadSamples (.-timeout js/ENV)))
  (loadSamples)

  )
