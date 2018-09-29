(ns muance.inspector-utils)

(defn highlight-node [mask node]
  (let [bounds (.localToScene node (.getBoundsInLocal node))]
    (.toFront mask)
    (.setVisible mask true)
    (.resizeRelocate
     mask
     (.getMinX bounds) (.getMinY bounds)
     (.getWidth bounds) (.getHeight bounds))))
