(ns muance.list-cell
  (:require [muance.arrays :as a]
            [muance.diff :as diff]
            [muance.vtree :as vtree])
  (:import [java.util ArrayList]
           [muance.javafx ListCell]))

(def ^:const index-vtree 0)
(def ^:const index-component 1)

(defn list-cell-updateItem [^ListCell this item empty]
  (if (or (nil? item) empty)
    (.setGraphic this nil)
    (let [vtree-component (.getVtreeComponent this)
          vtree (a/aget vtree-component index-vtree)
          render-queue (vtree/render-queue vtree)
          comp-fn (a/aget vtree-component index-component)
          vnode (vtree/vnode vtree)
          comp (a/aget (vtree/vnode vtree) diff/index-children 0)]
      (diff/patch-impl render-queue vnode comp comp-fn item false)
      (diff/process-post-render-hooks render-queue))))

