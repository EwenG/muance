(ns muance.j
  (:require [muance.javafx :as javafx]
            [muance.core :as m]
            [muance.j-impl :as j-impl]
            [muance.attributes :as attributes]))

(def javafx-nodes #{{:name 'group :tag 'javafx.scene.Group}
                    {:name 'v-box :tag 'javafx.scene.layout.VBox}
                    {:name 'h-box :tag 'javafx.scene.layout.HBox}
                    {:name 'pane :tag 'javafx.scene.layout.Pane}
                    {:name 'stack-pane :tag 'javafx.scene.layout.StackPane}
                    {:name 'flow-pane :tag 'javafx.scene.layout.FlowPane}
                    {:name 'border-pane :tag 'javafx.scene.layout.BorderPane}
                    {:name 'anchor-pane :tag 'javafx.scene.layout.AnchorPane}
                    {:name 'grid-pane :tag 'javafx.scene.layout.GridPane}
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
                    {:name 'table-column :tag 'muance.javafx.TableColumn
                     :children-getter 'getColumns}
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
                    {:name 'tool-bar :tag 'javafx.scene.control.ToolBar
                     :children-getter 'getItems}
                    {:name 'svg-path :tag 'javafx.scene.shape.SVGPath}
                    {:name 'path :tag 'javafx.scene.shape.Path}})

(def element-macros javafx-nodes)

(defmacro def-element-macros []
  `(do
     ~@(for [{:keys [name tag children-getter]} element-macros]
         `(javafx/make-element-macro
           ~name ~tag ~children-getter))))

(def-element-macros)

(defn tree-view-with-unmount [[k v :as attr]]
  (if (and (= k ::m/hooks) (map? v))
    (do
      (if-let [will-unmount (:will-unmount v)]
        [k (assoc v :will-unmount `(fn [props# state#]
                                     (~will-unmount props# state#)
                                     (javafx/tree-view-will-unmount props# state#)))]
        [k (assoc v :will-unmount `javafx/tree-view-will-unmount)]))
    attr))

(defmacro tree-view [& body]
  (let [body (attributes/map-attributes tree-view-with-unmount body)]
    `(j-impl/tree-view ~@body)))

(defn table-view-with-unmount [[k v :as attr]]
  (if (and (= k ::m/hooks) (map? v))
    (do
      (if-let [will-unmount (:will-unmount v)]
        [k (assoc v :will-unmount `(fn [props# state#]
                                     (~will-unmount props# state#)
                                     (javafx/table-view-will-unmount props# state#)))]
        [k (assoc v :will-unmount `javafx/table-view-will-unmount)]))
    attr))

(defmacro table-view [& body]
  (let [body (attributes/map-attributes table-view-with-unmount body)]
    `(j-impl/table-view ~@body)))

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
