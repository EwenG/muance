(ns muance.j-impl
  (:require [muance.javafx :as javafx]))

(javafx/make-element-macro tree-view javafx.scene.control.TreeView nil)
(javafx/make-element-macro table-view javafx.scene.control.TableView getColumns)

