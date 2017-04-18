(ns muance.todo
  (:require
   [muance.core :as m :include-macros true]
   [clojure.string])
  (:require-macros [muance.h :as h]))

(m/defcomp css []
  (h/link :rel "stylesheet" :type "text/css" :href "todo.css"))

(declare todo-state)

(defn add-todo* [{:keys [counter items] :as todo-state} text]
  (-> todo-state
      (assoc-in [:items (inc counter)] {:id (inc counter) :title text :done false})
      (assoc :counter (inc counter))))

(defn add-todo [text]
  (swap! todo-state add-todo* text))

(defn toggle [id] (swap! todo-state update-in [:items id :done] not))

(defn toggle-handler [e state-ref id]
  (toggle id))

(defn save [id title] (swap! todo-state assoc-in [:items id :title] title))

(defn delete [id] (swap! todo-state update :items dissoc id))

(defn delete-handler [e state-ref id]
  (delete id))

(defn complete-all* [items v]
  (->> (for [[id item] items]
         [id (assoc item :done v)])
       (into (empty items))))

(defn complete-all [v]
  (swap! todo-state update :items complete-all* v))

(defn complete-all-hanlder [e state-ref v]
  (complete-all v))

(defn clear-done* [items]
  (->> (for [[id item] items
             :when (not (:done item))]
         [id item])
       (into (empty items))))

(defn clear-done [e state-ref]
  (swap! todo-state update :items clear-done*))

(defn input-changed [e state-ref]
  (reset! state-ref (-> e .-target .-value)))

(defn input-save [e state-ref]
  (let [v (-> @state-ref str clojure.string/trim)]
    (when-not (empty? v) (add-todo v))
    (reset! state-ref "")))

(defn input-keydown [e state-ref]
  (case (.-which e)
    13 (input-save e state-ref)
    27 (reset! state-ref "")
    nil))

(m/defcomp todo-input []
  (h/input :id "new-todo" :placeholder "What needs to be done?"
           :type "text" :value m/*state*
           ::m/on [[:input input-changed]
                   [:keydown input-keydown]
                   [:blur input-save]]))

(defn edit-changed [e state-ref]
  (swap! state-ref assoc :val (-> e .-target .-value)))

(defn edit-save [e state-ref id]
  (let [v (-> @state-ref :val str clojure.string/trim)]
    (when-not (empty? v) (save id v))
    (swap! state-ref assoc :editing false)))

(defn edit-keydown [e state-ref id]
  (case (.-which e)
    13 (edit-save e state-ref id)
    27 (swap! state-ref assoc :editing false)
    nil))

(defn focus-node [props state]
  (.focus (m/dom-node m/*vnode*)))

(defn todo-edit [title id]
  (h/input :class "edit"
           :type "text" :value (str (:val m/*state*))
           ::m/on [[:input edit-changed]
                   [:keydown edit-keydown id]
                   [:blur edit-save id]]
           ::m/hooks {:did-mount focus-node}))

(defn filter-handler [e state-ref name]
  (swap! todo-state assoc :filt name))

(defn todo-stats-link [filt name text]
  (h/a
   :class (when (= name filt) "selected")
   ::m/on [:click filter-handler name]
   (m/text text)))

(m/defcomp todo-stats [{:keys [filt active done]}]
  (h/div
   (h/span
    :id "todo-count"
    (h/strong (m/text active))
    (m/text " " (case active 1 "item" "items") " left"))
   (h/ul
    :id "filters"
    (h/li (todo-stats-link filt :all "All"))
    (h/li (todo-stats-link filt :active "Active"))
    (h/li (todo-stats-link filt :done "Completed")))
   (when (pos? done)
     (h/button
      :id "clear-completed"
      ::m/on [:click clear-done] (m/text "Clear completed " done)))))

(defn set-editing [e state-ref b]
  (swap! state-ref assoc :editing b))

(m/defcomp todo-item [{:keys [id done title]}]
  (h/li
   :class [(when done "completed") (when (:editing m/*state*) "editing")]
   (h/div
    :class "view"
    (h/input
     :class "toggle" :type "checkbox" :checked done
     ::m/on [:change toggle-handler id])
    (h/label ::m/on [:dblclick set-editing true] (m/text title))
    (h/button
     :class "destroy"
     ::m/on [:click delete-handler id]))
   (when (:editing m/*state*)
     (todo-edit title id))))

(defn display-item? [{:keys [done]} filt]
  (case filt
    :active (not done)
    :done done
    :all true))

(m/defcomp todo-app [{:keys [items filt]}]
  (let [items (vals items)
        done (->> items (filter :done) count)
        active (- (count items) done)]
    (h/div
     (h/section
      :id "todoapp"
      (h/header
       :id "header"
       (h/h1 "todos")
       (todo-input)
       (when (-> items count pos?)
         (h/div
          (h/section
           :id "main"
           (h/input
            :id "toggle-all" :type "checkbox" :checked (zero? active)
            ::m/on [:change complete-all-hanlder (pos? active)])
           (h/label :for "toggle-all" "Mark all as complete")
           (h/ul
            :id "todo-list"
            (doseq [todo items
                    :when (display-item? todo filt)]
              (todo-item (:id todo) todo))))
          (h/footer
           :id "footer"
           (todo-stats {:active active :done done :filt filt}))))))
     (h/footer
      :id "info"
      (h/p "Double-click to edit a todo")))))

(def init-state {:counter 0
                 :items (sorted-map)
                 :filt :all})

(defonce todo-state (atom init-state))

(defonce css-vtree (m/vtree))
(m/append-child css-vtree (.-head js/document))
(m/patch css-vtree css)

(defonce vtree (m/vtree))
(m/append-child vtree (.-body js/document))
(m/patch vtree todo-app @todo-state)

(add-watch todo-state ::todo-app (fn [k r o n] (m/patch vtree todo-app n)))

(comment

  (m/remove css-vtree)
  (set! vtree (m/vtree))

  (reset! todo-state init-state)
  
  (add-todo "ee")
  (toggle (:counter @todo-state))
  (save (:counter @todo-state) "ee3")
  (delete (:counter @todo-state))
  
  
  )
