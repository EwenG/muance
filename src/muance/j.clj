(ns muance.j
  (:require [muance.javafx :as javafx]))

(def javafx-nodes #{{:name 'group :tag 'javafx.scene.Group}
                    {:name 'v-box :tag 'javafx.scene.layout.VBox}
                    {:name 'h-box :tag 'javafx.scene.layout.HBox}
                    {:name 'pane :tag 'javafx.scene.layout.Pane}
                    {:name 'stack-pane :tag 'javafx.scene.layout.StackPane}
                    {:name 'flow-pane :tag 'javafx.scene.layout.FlowPane}
                    {:name 'border-pane :tag 'javafx.scene.layout.BorderPane}
                    {:name 'anchor-pane :tag 'javafx.scene.layout.AnchorPane}
                    {:name 'scroll-pane :tag 'javafx.scene.control.ScrollPane
                     :children-getter 'contentProperty}
                    {:name 'split-pane :tag 'javafx.scene.control.SplitPane
                     :children-getter 'getItems}
                    {:name 'tab-pane :tag 'javafx.scene.control.TabPane
                     :children-getter 'getTabs}
                    {:name 'tab :tag 'javafx.scene.control.Tab
                     :children-getter 'contentProperty}
                    {:name 'list-view :tag 'javafx.scene.control.ListView
                     :children-getter 'getItems}
                    {:name 'tree-view :tag 'javafx.scene.control.TreeView
                     :children-getter 'rootProperty}
                    {:name 'tree-item :tag 'javafx.scene.control.TreeItem
                     :children-getter 'getChildren}
                    {:name 'text-flow :tag 'javafx.scene.text.TextFlow}
                    {:name 'text :tag 'javafx.scene.text.Text}
                    {:name 'image-view :tag 'javafx.scene.image.ImageView}
                    {:name 'button :tag 'javafx.scene.control.Button}
                    {:name 'text-field :tag 'javafx.scene.control.TextField}
                    {:name 'combo-box :tag 'javafx.scene.control.ComboBox
                     :children-getter 'getItems}
                    {:name 'label :tag 'javafx.scene.control.Label}
                    {:name 'checkbox :tag 'javafx.scene.control.CheckBox}
                    {:name 'toggle-button :tag 'javafx.scene.control.ToggleButton}
                    {:name 'circle :tag 'javafx.scene.shape.Circle}
                    {:name 'region :tag 'javafx.scene.layout.Region}
                    {:name 'hyperlink :tag 'javafx.scene.control.Hyperlink}
                    {:name 'menu-bar :tag 'javafx.scene.control.MenuBar
                     :children-getter 'getMenus}
                    {:name 'menu :tag 'javafx.scene.control.Menu
                     :children-getter 'getItems}
                    {:name 'menu-item :tag 'javafx.scene.control.MenuItem}
                    {:name 'svg-path :tag 'javafx.scene.shape.SVGPath}})

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
