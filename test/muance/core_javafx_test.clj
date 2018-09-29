(ns muance.core-javafx-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [muance.utils-test :as utils]
            [muance.core :as m]
            [muance.javafx :as javafx]
            [muance.j :as j]
            [muance.print :as mprint]
            [muance.inspector :as inspector])
  (:import [org.scenicview ScenicView]
           [muance.javafx TreeCell TreeCellFactory]
           [javafx.scene.layout BorderPane GridPane]))

(defn- init-stage []
  (let [root (javafx.scene.Group.)]
    (.setScene javafx/stage (javafx.scene.Scene. root 300 250))
    (.show javafx/stage)))

(defn scene ^javafx.scene.Scene []
  (-> javafx/stage (.getScene)))

(defonce vtree (atom nil))

(defn root-static-f []
  (j/group) (j/label :text "ef"))

(m/defcomp root-static-c []
  (root-static-f))

(deftest root-static []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree root-static-c))

(comment
  (javafx/run-later (init-stage))
  )


;;;;;;;;;;;;


(defn root []
  (-> (scene) (.getRoot)))

(defn post-render-hook []
  (prn "post-render"))

(defonce vtree (atom nil))

(m/defcomp split-pane-comp []
  (j/split-pane :orientation javafx.geometry.Orientation/VERTICAL
                (j/stack-pane
                 (j/button))
                (j/stack-pane
                 (j/tree-view
                  #_(j/tree-item :value "ee2"
                               (j/tree-item :value "ee3"))))))

(deftest split-pane []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree split-pane-comp))

(m/defcomp cell-comp [x]
  (j/h-box
   (j/text :text "ee")))

(m/defcomp combo-box-c [r]
  (j/v-box ::m/hooks {:will-mount (fn [props state]
                                    (.add ^java.util.List (.getStylesheets (m/node))
                                          (.toExternalForm
                                           (clojure.java.io/resource
                                            "css/combo-box.css")))
                                    (inspector/inspect
                                     (.getScene ^javafx.scene.Node (m/node))))
                      :will-unmount (fn [props state]
                                      (inspector/inspect-unmount
                                       (.getScene ^javafx.scene.Node (m/node))))}
           :id "vbox"
           :style {:-fx-cursor "hand"}
           #_(j/stack-pane
            :styleClass ["combo-box-base" "combo-box"]
            :style {:-fx-max-width "50.0"
                    :-fx-max-height "26.0"}
            (when (> r 50)
              (j/text :text (str r) :font (javafx.scene.text.Font. 20)
                      ::m/hooks {:did-mount (fn [props state]
                                              (def nn (m/node)))}))
            (j/text :text "abcdefghijabcdefghija bcdefghijabcdefghijabcde fghijabcdefghijabcdefghijabcd efghij abcdefghij abcdefghijabcd efghijabcdefghijabcdefghijabcde fghijabcdefghijabcdefghijabcdefghi jabcdefgh ijabcde fghijabcde fghij abcdefghijabcdefghija bcdefghijabcdefghijabcde fghijabcdefghijabcdefghijabcd efghij abcdefghij")
            (j/combo-box #_:items #_(doall (for [i (range r)] i))
                         #_#_::m/listen [:items (fn [] (.println System/out "e"))]
                         #_#_::m/key 1))
           (j/table-view
            :items ["e" "f"]
            (j/table-column
             ::m/hooks {:did-mount (fn [props state]
                                     (let [cell-factory (javafx/table-cell-factory cell-comp)]
                                       (.setCellFactory (m/node) cell-factory)
                                       #_(prn (.getCells cell-factory))))}))
           (j/grid-pane
            (j/stack-pane
             ::m/hooks {:did-mount (fn [props state]
                                     (GridPane/setColumnIndex (m/node) (int 1)))}
             :styleClass ["table-row-cell" "table-cell"]
             (j/hyperlink :text "css link")))))

(comment
  (combo-box)
  )

(deftest combo-box []
  (swap! vtree utils/new-vtree)
  (javafx/run-later (m/append-child (scene) @vtree))
  (m/patch @vtree combo-box-c 51))

(m/defcomp css-c []
  (j/stack-pane :style {:-fx-background-color "-fx-body-color"}
                #_#_:styleClass ["combo-box-base" "combo-box"]
                (j/button)))

(deftest css []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree css-c))

(m/defcomp list-item-c [x]
  (j/text :text x))

(m/defcomp list-view-c [x]
  (if x
    (j/list-view
     :editable false
     (doseq [e x]
       (j/text :text e)))
    (j/button)))

(deftest list-view []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree list-view-c (into [] (map str) (range 0 100))))

(m/defcomp chart-c
  ::m/hooks {:did-mount (fn [props state]
                          (let [chart (javafx.scene.chart.LineChart.
                                       (javafx.scene.chart.NumberAxis.)
                                       (javafx.scene.chart.NumberAxis.))
                                series (doto (javafx.scene.chart.XYChart$Series.)
                                         (.setName "Series1"))
                                data (let [coll [(javafx.scene.chart.XYChart$Data. 1 23)
                                                 (javafx.scene.chart.XYChart$Data. 2 14)]]
                                       (javafx.collections.FXCollections/observableArrayList
                                        ^java.util.Collection coll))]
                            (.setData series data)
                            (.add (.getData chart) series)
                            (.setRoot ^javafx.scene.Scene (m/parent-node) chart)))} 
  [])

(deftest chart []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree chart-c))

(defn tab-pane-close-request [e state-ref]
  (.consume ^javafx.event.Event e)
  (prn e state-ref))

(m/defcomp tab-pane-c [t]
  (j/tab-pane
   (j/tab ::m/on [:closeRequest tab-pane-close-request] "e")
   (j/tab (j/text :text t))))

(deftest tab-pane []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree tab-pane-c "g3"))

(defn make-timeline [^javafx.scene.Node canvas ^javafx.scene.shape.Circle ball dx dy]
  (let [dx (volatile! dx)
        dy (volatile! dy)]
    (javafx.animation.Timeline.
     ^"[Ljavafx.animation.KeyFrame;"
     (into-array javafx.animation.KeyFrame
                 [(javafx.animation.KeyFrame.
                   (javafx.util.Duration/millis 20) 
                   (reify javafx.event.EventHandler
                     (handle [this t]
                       (.setLayoutX ball (+ (.getLayoutX ball) @dx))
                       (.setLayoutY ball (+ (.getLayoutY ball) @dy))
                       (let [bounds (.getBoundsInLocal canvas)]
                         (when (or (<= (.getLayoutX ball) (+ (.getMinX bounds) (.getRadius ball)))
                                   (>= (.getLayoutX ball) (- (.getMaxX bounds) (.getRadius ball))))
                           (vreset! dx (- @dx)))
                         (when (or (>= (.getLayoutY ball) (- (.getMaxY bounds) (.getRadius ball)))
                                   (<= (.getLayoutY ball) (+ (.getMinY bounds) (.getRadius ball))))
                           (vreset! dy (- @dy))))))
                   ^"[Ljavafx.animation.KeyValue;"
                   (make-array javafx.animation.KeyValue 0))]))))

(m/defcomp moving-ball-c []
  (j/pane
   (let [canvas (m/node)]
     (j/circle :radius 10
               :fill javafx.scene.paint.Color/CADETBLUE
               ::m/hooks {:did-mount (fn [props state]
                                       (.relocate ^javafx.scene.shape.Circle (m/node) 5 5)
                                       (let [^javafx.animation.Timeline timeline
                                             (make-timeline canvas (m/node) 7 3)]
                                         (.setCycleCount
                                          timeline javafx.animation.Timeline/INDEFINITE)
                                         (m/set :timeline timeline)
                                         (.play timeline)))
                          :will-unmount (fn [props state]
                                          (.stop ^javafx.animation.Timeline (m/get :timeline)))}))))

(deftest moving-ball []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree moving-ball-c))

(m/defcomp region-c [{:keys [b]}]
  (j/region
   ::m/hooks {:did-mount (fn [props state]
                           (.resizeRelocate ^javafx.scene.Node (m/node) 0.0 0.0 1010.0 1000.0))}
   :style {:-fx-background-color "#6495ed80"}
   :managed false
   :mouseTransparent true
   :layoutX 0
   :layoutY 0))

(deftest region []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree region-c))

#_(m/defcomp menu-c []
  (j/split-pane :orientation javafx.geometry.Orientation/VERTICAL
   (j/v-box)
   (j/border-pane
    :prefWidth 1000
    (j/menu-bar
     :prefWidth 1000
     ::m/hooks {:did-mount (fn [props state]
                             (javafx.scene.layout.BorderPane/setAlignment
                              (m/node) javafx.geometry.Pos/TOP_CENTER))}
     (j/menu :text "mm"
             (j/menu-item :text "tt")))
    (j/stack-pane
     :prefWidth 1000
     ::m/hooks {:did-mount (fn [props state]
                             (javafx.scene.layout.BorderPane/setAlignment
                              (m/node) javafx.geometry.Pos/CENTER))}
     (j/button)))))

#_(m/defcomp menu-c []
  
  (j/flow-pane :orientation javafx.geometry.Orientation/VERTICAL
   (j/menu-bar
    #_#_::m/hooks {:did-mount (fn [props state]
                            (javafx.scene.layout.AnchorPane/setTopAnchor
                             (m/node) 0.0)
                            (javafx.scene.layout.AnchorPane/setLeftAnchor
                             (m/node) 0.0)
                            (javafx.scene.layout.AnchorPane/setRightAnchor
                             (m/node) 0.0)
                            #_(javafx.scene.layout.BorderPane/setAlignment
                               (m/node) javafx.geometry.Pos/TOP_LEFT))}
    ::m/hooks {:did-mount (fn [props state]
                            (.setPrefWidth (m/node) (.getWidth (m/parent-node))))}
    (j/menu :text "mm"
            (j/menu-item :text "tt")))
   (j/stack-pane
    #_#_::m/hooks {:did-mount (fn [props state]
                            (javafx.scene.layout.AnchorPane/setTopAnchor
                             (m/node) 0.0)
                            (javafx.scene.layout.AnchorPane/setBottomAnchor
                             (m/node) 0.0)
                            (javafx.scene.layout.AnchorPane/setLeftAnchor
                             (m/node) 0.0)
                            (javafx.scene.layout.AnchorPane/setRightAnchor
                             (m/node) 0.0)
                            #_(javafx.scene.layout.BorderPane/setAlignment
                               (m/node) javafx.geometry.Pos/CENTER))}
    :style {:-fx-background-color "#6495ed80"}
    (j/button))))

(m/defcomp menu-c []
  
  (j/border-pane
   ::m/hooks {:did-mount (fn [props state]
                           #_(.clear (.getChildren (m/node))))}
   (j/menu-bar
    ::m/hooks {:did-mount (fn [props state]
                            (javafx/set-position-in-border-pane-top))}
    (j/menu :text "mm"
            (j/menu-item :text "tt")))
   (j/stack-pane
    ::m/hooks {:did-mount (fn [props state]
                            (javafx/set-position-in-border-pane-center))}
    :style {:-fx-background-color "#6495ed80"}
    (j/button))))

(deftest menu []
  (swap! vtree utils/new-vtree)
  (m/append-child (scene) @vtree)
  (m/patch @vtree menu-c))

(m/defcomp svg-path-c []
  (j/stack-pane
   (j/svg-path :content "m 15.646484,0.00195313 c -8.6040707,0 -15.65039063,7.04436567 -15.65039025,15.64843787 v 67.685547 c 0,8.604056 7.04632825,15.648475 15.65039025,15.648437 H 26.195312 V 91.425781 H 15.646484 c -4.547114,0 -8.0898438,-3.542732 -8.0898434,-8.089843 V 15.650391 c 0,-4.547105 3.5427384,-8.0898441 8.0898434,-8.0898441 h 67.691407 c 4.547104,0 8.091797,3.5427391 8.091797,8.0898441 v 9.833984 h 7.558593 v -9.833984 c 0,-8.6040722 -7.046319,-15.64843787 -15.65039,-15.64843787 z M 34.351562,33.714844 45.380859,94.939453 47.210938,91.412109 C 51.619963,82.887593 56.42519,75.50736 62.291016,69.142578 l 25.380859,26.808594 6.333984,-5.994141 -25.43164,-26.859375 c 5.955327,-5.091423 12.929963,-9.407223 21.451172,-13.001953 l 3.736328,-1.580078 -3.935547,-0.982422 z")))

(deftest svg-path []
  (swap! vtree utils/new-vtree)
  (javafx/run-later (m/append-child (scene) @vtree))
  (m/patch @vtree svg-path-c))

(m/defcomp hit-c
  ::m/hooks {:did-mount (fn [props state]
                           (.add ^java.util.List (.getStylesheets (.getScene (m/node)))
                                 (.toExternalForm
                                  (clojure.java.io/resource
                                   "css/combo-box.css"))))}
  []
  (j/group
   (j/path
    ::m/hooks {:did-mount (fn [props state]
                            (.setStroke (m/node) nil))}
    :id "highlight"
    :style {:-fx-fill "-fx-accent"})
   (j/text-flow
    :cursor javafx.scene.Cursor/TEXT
    ::m/hooks {:will-mount (fn [props state]
                             (m/set :caretPos (atom -1))
                             (m/set :anchorPos (atom -1)))}
    ::m/on [[:mousePressed (fn [e state node {:keys [caretPos anchorPos]}]
                             (.requestFocus node)
                             (let [highlight (.lookup (.getScene node) "#highlight")
                                   text1 (.lookup node "#text1")
                                   text2 (.lookup node "#text2")
                                   hit (.hitTest node (javafx.geometry.Point2D.
                                                       (.getX e) (.getY e)))]
                               (reset! caretPos (.getInsertionIndex hit))
                               (reset! anchorPos (.getInsertionIndex hit))
                               (.clear ^java.util.List (.getElements highlight))
                               (.setSelectionStart text1 -1)
                               (.setSelectionEnd text1 -1)
                               (.setSelectionStart text2 1)
                               (.setSelectionEnd text2 6)))
             (m/node) {:caretPos (m/get :caretPos) :anchorPos (m/get :anchorPos)}]
            [:mouseDragged (fn [e state node {:keys [caretPos anchorPos]}]
                               (let [highlight (.lookup (.getScene node) "#highlight")
                                     text1 (.lookup node "#text1")
                                     text2 (.lookup node "#text2")
                                     hit (.hitTest node (javafx.geometry.Point2D.
                                                         (.getX e) (.getY e)))]
                                 (reset! caretPos (.getInsertionIndex hit))
                                 (.clear ^java.util.List (.getElements highlight))
                                 (when (and (>= @anchorPos 0) (not= @caretPos @anchorPos))
                                   (let [i1 (Math/min @caretPos @anchorPos)
                                         i2 (Math/max @caretPos @anchorPos)
                                         len1 (.length (.getText text1))]
                                     (if (< i1 len1)
                                       (do
                                         (.setSelectionStart text1 i1)
                                         (if (< i2 len1)
                                           (.setSelectionEnd text1 i2)
                                           (.setSelectionEnd text1 len1)))
                                       (do
                                         (.setSelectionStart text1 -1)
                                         (.setSelectionEnd text1 -1)))
                                     (if (> i2 (inc len1))
                                       (do
                                         (cond (< i1 (inc len1))
                                               (do
                                                 (.setSelectionStart text2 0))
                                               (>= i1 (inc len1))
                                               (do
                                                 
                                                 (.setSelectionStart text2 (- i1 len1 1))))
                                         (do
                                           (.setSelectionEnd text2 (- i2 len1 1))))
                                       (do
                                         (.setSelectionStart text2 -1)
                                         (.setSelectionEnd text2 -1)))
                                     (.addAll (.getElements highlight)
                                              (.rangeShape node i1 i2))))))
               (m/node) {:caretPos (m/get :caretPos) :anchorPos (m/get :anchorPos)}]]
    ::m/listen [:focused (fn [o n state node]
                           (when-not n
                             (let [highlight (.lookup (.getScene node) "#highlight")
                                   text1 (.lookup node "#text1")
                                   text2 (.lookup node "#text2")]
                               (.clear ^java.util.List (.getElements highlight))
                               (.setSelectionStart text1 -1)
                               (.setSelectionEnd text1 -1)
                               (.setSelectionStart text2 -1)
                               (.setSelectionEnd text2 -1))))
                (m/node)]
    (j/text :id "text1"
            :text "1 lorem ipsum\n"
            :selectionFill javafx.scene.paint.Color/WHITE)
    (j/text :id "text2"
            :text "2 lorem ipsum"
            :selectionFill javafx.scene.paint.Color/WHITE)
    (j/text-field :text "2 lorem ipsum")
    #_(j/text-field :text "2 lorem ipsum"))))

(m/defcomp hit-c2 [])

(deftest hit []
  (swap! vtree utils/new-vtree)
  (javafx/run-later (m/append-child (scene) @vtree))
  (m/patch @vtree hit-c))

(comment
  (javafx/run-later (init-stage))

  (m/patch @vtree hit-c2)
  
  (root-static)
  (split-pane)
  (combo-box)
  (css)
  (list-view)
  (chart)
  (tab-pane)
  (moving-ball)
  (region)
  (menu)
  (svg-path)
  (hit)
  
  (.setRoot (scene) (javafx.scene.Group.))
  (.getRoot (scene))
  (.getChildren (root))

  (mprint/format-render-queue @vtree)

  (. ^javafx.scene.control.TextField (javafx.scene.control.TextField.) focusedProperty)

  (loop [i 0
         b true]
    (when (< i 1000001)

      (m/patch @vtree root-static-c {:b b :f handler})
      (recur (inc i) (not b))))

  (loop [i 0
         b true]
    (when (< i 10)
      (javafx/run-later (prn 33))
      (recur (inc i) (not b))))

  (mprint/format-vtree @vtree)

  (.getStyle (.getRoot (scene)))

  (javafx/run-later (ScenicView/show (scene)))

  (.getStyleMapList (com.sun.javafx.css.StyleManager/getInstance))

  (.getSubScene (javafx.scene.Group.))

  )
