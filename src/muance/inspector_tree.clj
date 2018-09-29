(ns muance.inspector-tree
  (:require [muance.javafx :as javafx]
            [muance.core :as m]
            [muance.j :as j]
            [muance.diff :as diff]
            [muance.inspector-utils :as utils])
  (:import [javafx.scene Scene Parent Group Node Cursor]
           [javafx.scene.control SplitPane TreeItem TreeView]
           [javafx.scene.layout Region Pane StackPane HBox]
           [javafx.scene.input MouseEvent]
           [muance.javafx TreeCell TreeCellFactory]
           [java.util HashMap List]
           [java.lang.reflect Modifier Method]
           [javafx.beans Observable]
           [javafx.geometry BoundingBox Pos]
           [javafx.scene.paint Color]
           [javafx.scene.image ImageView Image]
           [javafx.event EventHandler]
           [javafx.scene.effect Light Lighting]
           [javafx.scene.shape SVGPath]
           [javafx.scene.text Font]
           [javafx.scene.layout Region Background BackgroundFill]))

(defmulti format-property (fn [item-value property-k property-v]
                            (class property-v)))

(defmethod format-property Object [item-value property-k property-v]
  (j/text :styleClass "tree-item-property-value"
          :text (pr-str property-v)))

(defmethod format-property nil [item-value property-k property-v]
  (j/text :styleClass "tree-item-property-value"
          :text "nil"))

(defmethod format-property Node [item-value property-k property-v]
  (j/text :styleClass "tree-item-property-value"
          :text (.getName (class property-v))))

(defn bounding-box-action [e state cell item-value property-k]
  (.startEdit cell)
  (.commitEdit cell (update-in item-value [property-k :opened] not)))

(defmethod format-property javafx.scene.text.Font
  [item-value property-k property-v]
  (let [opened (get-in item-value [property-k :opened])
        cell (javafx/get-cell (m/node))]
    (j/hyperlink ::m/on [:action bounding-box-action cell item-value property-k]
                 :text "Font"
                 :focusTraversable (when (.isExpanded (.getTreeItem cell)) true))
    (j/text :styleClass "tree-item-property-value"
            :text (if opened
                    (str {:name (.getName property-v)
                          :family (.getFamily property-v)
                          :style (.getStyle property-v)
                          :size (.getSize property-v)})
                    "{...}"))))

(defn format-property-entry [item-value k v]
  (j/text :styleClass "tree-item-property-key"
          :text (str k))
  (j/text :styleClass "tree-item-property-key"
          :text "=")
  (format-property item-value k v))

(defn format-properties [item-value properties-map]
  (doseq [[k v] (drop-last 1 properties-map)]
    (format-property-entry item-value k v)
    (j/text :text "  "))
  (when-let [[k v] (last properties-map)]
    (format-property-entry item-value k v)))

(m/defcomp tree-cell-comp [{:keys [node properties mask highlighted] :as item-value}]
  ;; mutating the treeItem state in mouseEntered / mouseExited does not work because cells nodes
  ;; are removed / added at any moment thus a mouseEntered does not involves a mouseExited
  ;; and vice versa
  ;; mouseEntered / mouseExited are not used anymore here but I keep it as a remainder ...
  (j/h-box
   :alignment Pos/CENTER_LEFT
   (j/text :styleClass "tree-item-type"
           :text (.getName (class node)))
   (j/text :text "  ")
   (format-properties item-value properties)))

(def get-observable-properties
  (memoize (fn [c]
             (let [methods (into [] (.getMethods c))]
               (for [m methods
                     :when (and
                            (.endsWith (.getName m) "Property")
                            (isa? (.getReturnType m) Observable)
                            (isa? (.getReturnType m) javafx.beans.value.WritableValue)
                            (Modifier/isPublic (.getModifiers m)))]
                 m)))))

(comment
  (map #(.getName %) (get-observable-properties javafx.scene.control.TreeView))
  (map #(.getName %) (get-observable-properties javafx.scene.text.Text))
  )

(defn add-properties-listeners [tree-item properties-methods]
  (try
    (let [node-with-properties (.getValue tree-item)
          node (:node node-with-properties)
          properties (:properties node-with-properties)]
      (doseq [property-method properties-methods]
        (let [method-name (.getName property-method)
              property-name (.substring method-name 0 (- (count method-name) (count "Property")))
              property (.invoke ^Method property-method node (make-array Object 0))]
          (.addListener
           property
           (reify javafx.beans.value.ChangeListener
             (changed [this observable o n]
               (let [node-with-properties (.getValue tree-item)
                     properties (:properties node-with-properties)]
                 (if diff/*vnode*
                   (do
                     ;; notify update
                     (.put properties property-name n)
                     (.setValue tree-item (into {} node-with-properties)))
                   (when (.remove properties property-name)
                     (.setValue tree-item (into {} node-with-properties)))))))))))
    (catch IllegalAccessException e
      nil)))

(defn make-tree-item [node mask]
  (let [node-with-properties {:node node :properties (HashMap.) :mask mask}
        tree-item (TreeItem. node-with-properties)
        children (.getChildren tree-item)]
    (when (instance? Parent node)
      (let [node-children (.getChildrenUnmodifiable ^Parent node)]
        (.clear children)
        (doseq [node-child node-children]
          (.add children (make-tree-item node-child mask)))
        (.addListener node-children
                      (reify javafx.collections.ListChangeListener
                        (onChanged [this c]
                          (while (.next c)
                            (cond (.wasPermutated c)
                                  (let [copy (.clone children)]
                                    (doseq [i (range (.getFrom c) (.getTo c))]
                                      (.set children i (.get copy (.getPermutation c i)))))
                                  (.wasUpdated c)
                                  nil
                                  :else
                                  (do
                                    (when (.wasRemoved c)
                                      (let [from (.getFrom c)]
                                        (.remove children from (+ from (.getRemovedSize c)))))
                                    (when (.wasAdded c)
                                      (let [from (.getFrom c)
                                            added (.getAddedSubList c)]
                                        (if (< from (.size children))
                                          (doseq [n added]
                                            (.add children from (make-tree-item n mask)))
                                          (doseq [n added]
                                            (.add children (make-tree-item n mask))))))))))))))
    (let [properties-methods (get-observable-properties (class node))]
      (add-properties-listeners tree-item properties-methods))
    tree-item))

(defn tree-cell-mouse-entered [this mask]
  (when-let [tree-item (.getTreeItem this)]
    (when-let [node (:node (.getValue tree-item))]
      (utils/highlight-node mask node)
      (let [^List style-classes (.getStyleClass this)]
        (when (not (.contains style-classes "highlighted"))
          (.add style-classes "highlighted"))))))

(defn tree-cell-mouse-exited [this mask]
  (.setVisible mask false)
  (.remove ^List (.getStyleClass this) "highlighted"))

#_(defn tree-cell-constructor [mask this]
  (let [node (Group.)
        vtree (javafx/vtree {;; The screen sometimes blinks when rendering asynchronously
                             :synchronous? true})]
    (.setOnMouseEntered this (reify EventHandler
                               (handle [_ e]
                                 (tree-cell-mouse-entered this mask))))
    (.setOnMouseExited this (reify EventHandler
                              (handle [_ e]
                                (tree-cell-mouse-exited this mask))))
    (.setState this {:vtree vtree :node node :mask mask})
    (m/append-child node vtree)
    (.setContentDisplay this ContentDisplay/GRAPHIC_ONLY)))

(defn tree-cell-constructor [mask this o]
  (.setOnMouseEntered this (reify EventHandler
                             (handle [_ e]
                               (tree-cell-mouse-entered this mask))))
  (.setOnMouseExited this (reify EventHandler
                            (handle [_ e]
                              (tree-cell-mouse-exited this mask)))))

#_(defn tree-cell-update-item [this item empty]
  (let [^List style-classes (.getStyleClass this)]
    (if (:highlighted item)
      (when (not (.contains style-classes "highlighted"))
        (.add style-classes "highlighted"))
      (.remove style-classes "highlighted"))
    (if (or empty (nil? item))
      (.setGraphic this nil)
      (let [{:keys [node vtree mask]} (.getState this)]
        (.setGraphic this node)
        (m/patch vtree tree-cell-comp (assoc item :cell this :mask mask))))))

(defn tree-cell-update-item [this item empty]
  (let [^List style-classes (.getStyleClass this)]
    (if (:highlighted item)
      (when (not (.contains style-classes "highlighted"))
        (.add style-classes "highlighted"))
      (.remove style-classes "highlighted"))))

#_(defn tree-cell-destructor [this]
  (let [{:keys [vtree]} (.getState this)]
    (m/unmount vtree)
    (m/remove vtree)))

#_(defn tree-view-did-mount [{:keys [root mask]} state]
  (.setEditable (m/node) true)
  (.setRoot (m/node) (make-tree-item root mask))
  (let [cell-factory (TreeCellFactory. tree-cell-update-item
                                       (partial tree-cell-constructor mask)
                                       tree-cell-destructor)]
    (.setCellFactory (m/node) cell-factory)))

(defn tree-view-did-mount [{:keys [root mask]} state]
  (.setEditable (m/node) true)
  (.setRoot (m/node) (make-tree-item root mask))
  (let [cell-factory (javafx/tree-cell-factory
                      tree-cell-comp
                      tree-cell-update-item
                      (partial tree-cell-constructor mask)
                      nil)]
    (.setCellFactory (m/node) cell-factory)))

#_(defn tree-view-will-unmount [{:keys [root]} state]
  (when-let [cell-factory (.getCellFactory (m/node))]
    (doseq [cell (.getCells cell-factory)]
      (.destroy cell))))

(defn items-to-expand [tree-item]
  (when tree-item
    (->> (iterate #(.getParent %) (.getParent tree-item))
         (take-while some?)
         (filter #(not (.isExpanded %))))))

(defn tree-view-inspected-item [tree-view inspected-node]
  (loop [tree-item (.getRoot tree-view)
         up? false]
    (if up?
      (if-let [next-sibling (.nextSibling tree-item)]
        (recur next-sibling false)
        (recur (.getParent tree-item) true))
      (when tree-item
        (let [{:keys [node]} (.getValue tree-item)]
          (if (identical? node inspected-node)
            tree-item
            (let [^List children (.getChildren tree-item)]
              (if (and children (not (.isEmpty children)))
                (recur (.get children 0) false)
                (if-let [next-sibling (.nextSibling tree-item)]
                  (recur next-sibling false)
                  (recur (.getParent tree-item) true))))))))))

(defn tree-view-did-update [{:keys [inspected-nodes]} state]
  (let [inspected-node (peek inspected-nodes)
        prev-inspected-node (m/get :inspected-node)]
    (when-not (identical? prev-inspected-node inspected-node)
      (m/set :inspected-node inspected-node)
      (when-let [prev-inspected-item (m/get :inspected-item)]
        (.setValue prev-inspected-item (dissoc (.getValue prev-inspected-item) :highlighted)))
      (doseq [expanded-item (m/get :expanded-items)]
        (.setExpanded expanded-item false))
      (m/unset :expanded-items)
      (when inspected-node
        (let [inspected-item (tree-view-inspected-item (m/node) inspected-node)
              items-to-expand (items-to-expand inspected-item)]
          (when inspected-item
            (doseq [item items-to-expand]
              (.setExpanded item true))
            (m/set :inspected-item inspected-item)
            (m/set :expanded-items items-to-expand)
            (.setValue inspected-item (assoc (.getValue inspected-item) :highlighted true))
            (when-let [inspected-index (.getRow (m/node) inspected-item)]
              (.scrollTo (m/node) inspected-index))))))))


;; make a component in j.clj that wraps tree-view with a unmount hook
