(ns muance.j
  (:require [muance.javafx :as javafx]))

(def javafx-nodes #{{:name 'text :tag 'javafx.scene.text.Text}
                    {:name 'stack-pane :tag 'javafx.scene.layout.StackPane}
                    {:name 'scroll-pane :tag 'javafx.scene.control.ScrollPane
                     :children-getter 'contentProperty}
                    {:name 'group :tag 'javafx.scene.Group}
                    {:name 'button :tag 'javafx.scene.control.Button}
                    {:name 'text-field :tag 'javafx.scene.control.TextField}
                    {:name 'split-pane :tag 'javafx.scene.control.SplitPane
                     :children-getter 'getItems}
                    {:name 'tree-view :tag 'javafx.scene.control.TreeView
                     :children-getter 'rootProperty}
                    {:name 'tree-item :tag 'javafx.scene.control.TreeItem
                     :children-getter 'getChildren}
                    {:name 'combo-box :tag 'javafx.scene.control.ComboBox
                     :children-getter 'getItems}
                    {:name 'list-view :tag 'javafx.scene.control.ListView}})

(def element-macros javafx-nodes)

(defmacro def-element-macros []
  `(do
     ~@(for [{:keys [name tag children-getter]} element-macros]
         `(javafx/make-element-macro
           ~name ~tag ~children-getter))))

(def-element-macros)

(comment
  (macroexpand-1 '(javafx/make-element-macro scroll-pane javafx.scene.control.ScrollPane contentProperty))
  (macroexpand-1 '(javafx/make-element-macro group javafx.scene.Group nil))
  (macroexpand-1 '(javafx/make-element-macro button javafx.scene.control.Button nil))
  (macroexpand '(scroll-pane))
  
  )

(comment
  (binding [*compile-path* "target/classes"]
    (compile 'muance.j))
  )
