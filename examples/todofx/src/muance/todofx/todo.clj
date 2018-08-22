(ns muance.todofx.todo
  (:require [muance.core :as m]
            [muance.javafx :as javafx]
            [muance.j :as j]))

(defn- init-stage []
  (let [root (javafx.scene.Group.)]
    (.setTitle javafx/stage "Todo app")
    (.setMaximized javafx/stage true)
    (.setScene javafx/stage (javafx.scene.Scene. root 300 250))
    (.show javafx/stage)))

(javafx.scene.text.Font/loadFont
 (.toExternalForm (clojure.java.io/resource "fontawesome-webfont.ttf"))
 10.0)

(declare todo-state)

(defn add-todo* [{:keys [counter items] :as todo-state} text]
  (-> todo-state
      (assoc-in [:items (inc counter)] {:id (inc counter) :title text :done false})
      (assoc :counter (inc counter))))

(defn add-todo [text]
  (swap! todo-state add-todo* text))

(defn toggle [id done] (swap! todo-state assoc-in [:items id :done] done))

(defn toggle-listener [o n state-ref id]
  (toggle id n))

(defn save [id title]
  (swap! todo-state assoc-in [:items id :title] title))

(defn delete [id] (swap! todo-state update :items dissoc id))

(defn delete-handler [e state-ref id]
  (delete id))

(defn complete-all* [items v]
  (->> (for [[id item] items]
         [id (assoc item :done v)])
       (into (empty items))))

(defn complete-all [v]
  (swap! todo-state update :items complete-all* v))

(defn input-changed [o n state-ref]
  (reset! state-ref n))

(defn input-save [e state-ref]
  (let [v (-> @state-ref str clojure.string/trim)]
    (when-not (empty? v) (add-todo v))
    (reset! state-ref "")))

(defn complete-all-handler [o n state-ref]
  (complete-all n))

(defn input-keydown [e state-ref]
  (when (= javafx.scene.input.KeyCode/ESCAPE (.getCode e))
    (reset! state-ref "")))

(m/defcomp todo-input []
  (j/text-field :id "addInput" :promptText "What needs to be done?"
                :text (m/state)
                ::m/hooks {:did-mount (fn [props state]
                                        (javafx.scene.layout.HBox/setHgrow
                                         (m/node) javafx.scene.layout.Priority/ALWAYS))}
                ::m/listen [[:text input-changed]]
                ::m/on [[:action input-save]
                        [:keyPressed input-keydown]]))

(defn set-editing [e state-ref title stack-pane-node]
  (when (> (.getClickCount e) 1)
    (m/with-post-render #(.requestFocus (.lookup stack-pane-node "#contentInput"))
      (swap! state-ref assoc
             :editing true
             :val title))))

(defn edit-changed [o n state-ref]
  (swap! state-ref assoc :val n))

(defn edit-save [e state-ref id]
  (let [v (-> @state-ref :val str clojure.string/trim)]
    (when-not (empty? v) (save id v))
    (swap! state-ref assoc :editing false)))

(defn edit-focused-listener [o n state-ref id]
  (when-not n
    (let [v (-> @state-ref :val str clojure.string/trim)]
      (when-not (empty? v) (save id v))
      (swap! state-ref assoc :editing false))))

(m/defcomp todo-item [{:keys [id done title]}]
  (j/h-box
   :id "root"
   :alignment javafx.geometry.Pos/CENTER_LEFT
   :minHeight Double/NEGATIVE_INFINITY
   :styleClass "item_root"
   ::m/listen [:hover (fn [o n state-ref]
                        (swap! state-ref assoc :hover n))]
   ::m/hooks {:did-mount (fn [props state]
                           (javafx.scene.layout.VBox/setVgrow
                            (m/node) javafx.scene.layout.Priority/ALWAYS))}
   (j/checkbox :id "completed" :mnemonicParsing false :selected done
               ::m/listen [:selected toggle-listener id])
   (j/stack-pane
    :alignment javafx.geometry.Pos/CENTER_LEFT
    ::m/hooks {:did-mount (fn [props state]
                            (javafx.scene.layout.HBox/setHgrow
                             (m/node) javafx.scene.layout.Priority/ALWAYS))}
    (let [stack-pane-node (m/node)]
      (j/h-box
       :id "contentBox" :styleClass "content_box"
       :visible (not (:editing (m/state)))
       ::m/on [:mouseClicked set-editing title stack-pane-node]
       (j/label :id "contentLabel"
                :maxHeight 1.7976931348623157E308
                :maxWidth 1.7976931348623157E308
                :text title
                ::m/hooks {:did-mount (fn [comp props]
                                        (javafx.scene.layout.HBox/setHgrow
                                         (m/node) javafx.scene.layout.Priority/ALWAYS))})
       (j/button :id "deleteButton" :mnemonicParsing false :visible (boolean (:hover (m/state)))
                 :styleClass "close_icon"
                 :text "\uf00d"
                 ::m/on [:action delete-handler id])))
    (j/text-field :id "contentInput" :promptText "What needs to be done?"
                  :visible (boolean (:editing (m/state)))
                  :text (str (:val (m/state)))
                  ::m/on [:action edit-save id]
                  ::m/listen [[:focused edit-focused-listener id]
                              [:text edit-changed]]))))

(defn filter-handler [e state-ref name]
  (swap! todo-state assoc :filt name))

(defn control-button [toggle-group filt name text]
  (j/toggle-button
   :mnemonicParsing false
   :selected (= name filt)
   :text text
   :toggleGroup toggle-group
   ::m/on [:action filter-handler name]))

(m/defcomp controls [{:keys [active filt]}]
  (j/h-box
   :alignment javafx.geometry.Pos/CENTER
   :spacing 20.0
   :padding (javafx.geometry.Insets. 5.0 5.0 5.0 5.0)
   (j/label
    :id "itemsLeftLabel" :text (str active " " (if (> active 1) "items" "item") " left"))
   (j/h-box
    :maxHeight Double/NEGATIVE_INFINITY
    :maxWidth Double/NEGATIVE_INFINITY
    :minHeight Double/NEGATIVE_INFINITY
    :minWidth Double/NEGATIVE_INFINITY
    :spacing 20.0
    :padding (javafx.geometry.Insets. 5.0 5.0 5.0 5.0)
    (let [toggle-group (javafx.scene.control.ToggleGroup.)]
      (control-button toggle-group filt :all "All")
      (control-button toggle-group filt :active "Active")
      (control-button toggle-group filt :done "Completed")))))

(defn display-item? [{:keys [done]} filt]
  (case filt
    :active (not done)
    :done done
    :all true))

(m/defcomp todo-app [{:keys [items filt]}]
  (let [items (vals items)
        done (->> items (filter :done) count)
        active (- (count items) done)]
    (j/v-box
     :alignment javafx.geometry.Pos/CENTER
     :maxHeight Double/NEGATIVE_INFINITY :maxWidth Double/NEGATIVE_INFINITY
     :minHeight Double/NEGATIVE_INFINITY :minWidth Double/NEGATIVE_INFINITY
     (j/label :id "title" :text "todos" :alignment javafx.geometry.Pos/CENTER_LEFT)
     (j/h-box
      :styleClass "add_item_root"
      :alignment javafx.geometry.Pos/CENTER_LEFT
      (j/checkbox
       :id "selectAll"
       :mnemonicParsing false
       :visible (not (empty? items))
       :selected (zero? active)
       ::m/listen [:selected complete-all-handler])
      (todo-input))
     (j/anchor-pane
      :maxHeight Double/MAX_VALUE
      :maxWidth Double/MAX_VALUE
      :minHeight Double/NEGATIVE_INFINITY
      :minWidth Double/NEGATIVE_INFINITY
      :style {:-fx-background-color "grey"
              :-fx-border-color "-fx-box-border"
              :-fx-border-width "1 0 1 0"}
      ::m/hooks {:did-mount (fn [props state]
                              (javafx.scene.layout.VBox/setVgrow
                               (m/node) javafx.scene.layout.Priority/ALWAYS))}
      (j/list-view
       :id "items"
       ::m/hooks {:did-mount (fn [comp props]
                               (javafx.scene.layout.AnchorPane/setTopAnchor (m/node) 0.0)
                               (javafx.scene.layout.AnchorPane/setBottomAnchor (m/node) 0.0)
                               (javafx.scene.layout.AnchorPane/setLeftAnchor (m/node) 0.0)
                               (javafx.scene.layout.AnchorPane/setRightAnchor (m/node) 0.0))}
       (doseq [todo items
               :when (display-item? todo filt)]
         (todo-item (:id todo) todo))))
     (controls {:active active :filt filt}))))

(defonce vtree (javafx/vtree))

(def init-state {:counter 0
                 :items (sorted-map)
                 :filt :all})

(defonce todo-state (atom init-state))

(add-watch todo-state ::todo-app (fn [k r o n] (m/patch vtree todo-app n)))

(comment
  (javafx/run-later (init-stage))
  (m/append-child (.getScene javafx/stage) vtree)

  (do
    (m/patch vtree todo-app @todo-state)
    (javafx/run-later
     (.clear ^java.util.List (.getStylesheets (.getScene javafx/stage)))
     (.add ^java.util.List (.getStylesheets (.getScene javafx/stage))
           (.toExternalForm (clojure.java.io/resource "muance/todofx/todo.css")))))
  )
