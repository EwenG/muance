(ns muance.inspector
  (:require [muance.javafx :as javafx]
            [muance.core :as m]
            [muance.j :as j]
            [muance.diff :as diff]
            [muance.arrays :as a]
            [muance.context :as context]
            [muance.inspector-icons :as icons]
            [muance.inspector-tree :as tree]
            [muance.inspector-properties :as properties]
            [muance.inspector-utils :as utils])
  (:import [javafx.scene Scene Parent Group Node Cursor]
           [javafx.scene.control SplitPane TreeItem TreeView ContentDisplay]
           [javafx.scene.layout Region Pane StackPane]
           [javafx.scene.input MouseEvent]
           [muance.javafx TreeCell TreeCellFactory]
           [java.util HashMap List]
           [javafx.beans Observable]
           [javafx.geometry BoundingBox]
           [javafx.scene.paint Color]
           [javafx.scene.image ImageView Image]
           [javafx.event EventHandler]
           [javafx.scene.effect Light Lighting]
           [javafx.scene.shape SVGPath]
           [javafx.scene.text Font]
           [javafx.scene.layout Region Background BackgroundFill]))

(defonce inspector-states (atom nil))

(defn get-inspector-state [{:keys [inspector-id]}]
  (get @inspector-states inspector-id))

(defn make-mask []
  (let [mask (Region.)]
    (.setStyle mask "-fx-background-color: #6495ed80;")
    (.setManaged mask false)
    (.setMouseTransparent mask true)
    (.setVisible mask false)
    mask))

(defn inspect-element-action
  [e state
   {:keys [scene root mask
           exited-event-filter
           entered-event-filter
           clicked-event-filter]
    :as props}]
  (let [{:keys [inspect-element]} (swap! state update :inspect-element not)]
    (if inspect-element
      (do
        (.addEventFilter root MouseEvent/MOUSE_EXITED_TARGET exited-event-filter)
        (.addEventFilter root MouseEvent/MOUSE_ENTERED_TARGET entered-event-filter)
        (.addEventFilter root MouseEvent/MOUSE_CLICKED clicked-event-filter))
      (let [inspector-state (get-inspector-state props)]
        (.removeEventFilter root MouseEvent/MOUSE_EXITED_TARGET exited-event-filter)
        (.removeEventFilter root MouseEvent/MOUSE_ENTERED_TARGET entered-event-filter)
        (.removeEventFilter root MouseEvent/MOUSE_CLICKED clicked-event-filter)
        (swap! inspector-state dissoc :inspected-nodes)))
    (.setVisible mask false)))

(defn on-inspect-element-event-entered [this e {:keys [mask]} inspector-state]
  (let [{:keys [inspected-nodes]} (swap! inspector-state update
                                         :inspected-nodes (fnil conj []) (.getTarget e))]
    (when-let [inspected-node (peek inspected-nodes)]
      (utils/highlight-node mask inspected-node))))

(defn make-inspect-element-event-filter-entered [props]
  (let [inspector-state (get-inspector-state props)]
    (reify EventHandler
      (handle [this e]
        (on-inspect-element-event-entered this e props inspector-state)))))

(defn exited-pop [stack e]
  (loop [stack stack]
    (when (seq stack)
      (let [candidate (peek stack)]
        (if (= candidate e)
          (pop stack)
          (recur (pop stack)))))))

(defn on-inspect-element-event-exited [this e {:keys [mask]} inspector-state]
  (let [{:keys [inspected-nodes]} (swap! inspector-state update
                                         :inspected-nodes exited-pop (.getTarget e))]
    (when-let [inspected-node (peek inspected-nodes)]
      (utils/highlight-node mask inspected-node))))

(defn make-inspect-element-event-filter-exited [props]
  (let [inspector-state (get-inspector-state props)]
    (reify EventHandler
      (handle [this e]
        (on-inspect-element-event-exited this e props inspector-state)))))

(defn tree-view-do-inspect [tree-view inspected-node]
  (let [inspected-item (tree/tree-view-inspected-item tree-view inspected-node)
        selection-model (.getSelectionModel tree-view)
        focus-model (.getFocusModel tree-view)]
    (when inspected-item
      (.select selection-model inspected-item)
      (when-let [selected-index (.getSelectedIndex selection-model)]
        (.requestFocus tree-view)
        (.focus focus-model selected-index)))))

(defn on-inspect-element-event-clicked [{:keys [scene root mask] :as props}
                                        entered-event-filter exited-event-filter
                                        this e state]
  (.removeEventFilter root MouseEvent/MOUSE_ENTERED_TARGET entered-event-filter)
  (.removeEventFilter root MouseEvent/MOUSE_EXITED_TARGET exited-event-filter)
  (.removeEventFilter root MouseEvent/MOUSE_CLICKED this)
  (swap! state dissoc :inspect-element)
  (let [inspected-node (.getTarget e)
        tree-view (.lookup scene "#inspector-tree-view")]
    (m/with-post-render #(tree-view-do-inspect tree-view inspected-node)
      (swap! (get-inspector-state props) dissoc :inspected-nodes)))
  (.setVisible mask false))

(defn make-inspect-element-event-filter-clicked
  [props entered-event-filter exited-event-filter]
  (javafx/event-handler
   (partial on-inspect-element-event-clicked props
            entered-event-filter exited-event-filter)))

(defn inspector-vtree-mount [props state]
  (.add ^List (.getStylesheets (m/node))
        (.toExternalForm (clojure.java.io/resource "css/inspector.css"))))

(defn tree-view-did-mount [props state]
  (let [inspector-state (get-inspector-state props)
        selection-model (.getSelectionModel (m/node))]
    (.addListener (.selectedItemProperty selection-model)
                  (javafx/change-listener
                   (fn [this observable o n state-ref]
                     (swap! inspector-state assoc :selected-item n))))
    (tree/tree-view-did-mount props state)))

(m/defcomp inspector-pane
  ::m/hooks {:did-mount inspector-vtree-mount}
  [{:keys [selected-item] :as props}]
  (j/border-pane
   (j/tool-bar
    ::m/hooks {:did-mount (fn [props state]
                            (javafx/set-position-in-border-pane-top (m/node)))}
    (j/button ::m/hooks {:will-mount (fn [props state]
                                       (let [entered-event-filter
                                             (make-inspect-element-event-filter-entered props)
                                             exited-event-filter
                                             (make-inspect-element-event-filter-exited props)]
                                         (m/set ::entered-event-filter entered-event-filter)
                                         (m/set ::exited-event-filter exited-event-filter)
                                         (m/set ::clicked-event-filter
                                                (make-inspect-element-event-filter-clicked
                                                 props entered-event-filter exited-event-filter))))
                         :did-mount (fn [props state]
                                      (.setGraphic
                                       (m/node)
                                       (javafx/icon
                                        icons/inspect-element
                                        {:width 20 :height 20})))
                         :will-unmount (fn [props state]
                                         (.removeEventFilter (:root props)
                                                             MouseEvent/MOUSE_ENTERED_TARGET
                                                             (m/get ::entered-event-filter))
                                         (.removeEventFilter (:root props)
                                                             MouseEvent/MOUSE_EXITED_TARGET
                                                             (m/get ::exited-event-filter))
                                         (.removeEventFilter (:root props)
                                                             MouseEvent/MOUSE_CLICKED
                                                             (m/get ::clicked-event-filter)))}
              ::m/on [:action inspect-element-action
                      (assoc props
                             :entered-event-filter (m/get ::entered-event-filter)
                             :exited-event-filter (m/get ::exited-event-filter)
                             :clicked-event-filter (m/get ::clicked-event-filter))]
              :styleClass (when (:inspect-element (m/state)) ["active"])))
   #_(j/flow-pane
    ::m/hooks {:did-mount (fn [props state]
                            (javafx/set-position-in-border-pane-top (m/node)))}
    (j/button ::m/hooks {:will-mount (fn [props state]
                                       (let [entered-event-filter
                                             (make-inspect-element-event-filter-entered props)
                                             exited-event-filter
                                             (make-inspect-element-event-filter-exited props)]
                                         (m/set ::entered-event-filter entered-event-filter)
                                         (m/set ::exited-event-filter exited-event-filter)
                                         (m/set ::clicked-event-filter
                                                (make-inspect-element-event-filter-clicked
                                                 props entered-event-filter exited-event-filter))))
                         :did-mount (fn [props state]
                                      (.setGraphic
                                       (m/node)
                                       (javafx/icon
                                        icons/inspect-element
                                        {:width 20 :height 20})))
                         :will-unmount (fn [props state]
                                         (.removeEventFilter (:root props)
                                                             MouseEvent/MOUSE_ENTERED_TARGET
                                                             (m/get ::entered-event-filter))
                                         (.removeEventFilter (:root props)
                                                             MouseEvent/MOUSE_EXITED_TARGET
                                                             (m/get ::exited-event-filter))
                                         (.removeEventFilter (:root props)
                                                             MouseEvent/MOUSE_CLICKED
                                                             (m/get ::clicked-event-filter)))}
              ::m/on [:action inspect-element-action
                      (assoc props
                             :entered-event-filter (m/get ::entered-event-filter)
                             :exited-event-filter (m/get ::exited-event-filter)
                             :clicked-event-filter (m/get ::clicked-event-filter))]
              :styleClass (when (:inspect-element (m/state)) ["active"])))
   (j/split-pane
    ::m/hooks {:did-mount (fn [props state]
                            (javafx/set-position-in-border-pane-center (m/node)))}
    (j/tree-view
     ::m/hooks {:did-mount tree-view-did-mount
                :did-update tree/tree-view-did-update}
     :id "inspector-tree-view")
    (when selected-item
      (properties/properties-pane selected-item)))))

(declare inspect-unmount)

(defn inspect [scene]
  (assert (javafx.application.Platform/isFxApplicationThread))
  (let [inspector-id (java.util.UUID/randomUUID)
        properties ^java.util.Map (.getProperties scene)]
    (when (.get properties "__muance__inspector__vtree__")
      (inspect-unmount scene))
    (let [root (.getRoot scene)
          split-pane (SplitPane.)
          split-pane-with-mask (StackPane.)
          mask (make-mask)
          inspector-vtree (javafx/vtree)
          inspector-state (atom {:root root
                                 :scene scene
                                 :mask mask
                                 :inspector-id inspector-id})]
      (.put properties "__muance__inspector__vtree__" inspector-vtree)
      (.put properties "__muance__inspector__root__" root)
      (.put properties "__muance__inspector__id__" inspector-id)
      (swap! inspector-states assoc inspector-id inspector-state)
      ;; Without this binding, it seems that the split-pane resize its content when its content
      ;; is very large
      (.bind (.minWidthProperty root) (.widthProperty split-pane))
      (.setOrientation split-pane javafx.geometry.Orientation/VERTICAL)
      (.add (.getChildren split-pane-with-mask) root)
      (.add (.getChildren split-pane-with-mask) mask)
      (.add (.getItems split-pane) split-pane-with-mask)
      (m/append-child split-pane inspector-vtree)
      (m/patch inspector-vtree inspector-pane @inspector-state)
      (add-watch inspector-state ::inspector-state-watch
                 (fn [r k o n]
                   (m/patch inspector-vtree inspector-pane n)))
      (.setRoot scene split-pane))))

(defn inspect-unmount [scene]
  (assert (javafx.application.Platform/isFxApplicationThread))
  (let [properties ^java.util.Map (.getProperties scene)]
    (when-let [inspector-id (.get properties "__muance__inspector__id__")]
      (remove-watch (get @inspector-states inspector-id) ::inspector-state-watch)
      (swap! inspector-states dissoc inspector-id))
    (when-let [inspector-vtree (.get properties "__muance__inspector__vtree__")]
      (m/unmount inspector-vtree)
      (m/remove inspector-vtree))
    (when-let [root (.get properties "__muance__inspector__root__")]
      ;; clear the root because "root" may already be in the node tree
      (let [parent (.getParent root)]
        (when parent
          (context/remove-node parent root)
          (.setRoot scene root))))
    (.remove properties "__muance__inspector__id__")
    (.remove properties "__muance__inspector__root__")
    (.remove properties "__muance__inspector__vtree__")))

;; TreeView requestFocus behavior - it seems that nodes inside cells sometimes do not have a scene, thus hey lose their focus, the focus is then set by default somewhere else.

;; Ensure the root is a layout, else use a detached window
;; filter out read-only properties / event handers
