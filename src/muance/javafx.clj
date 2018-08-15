(ns muance.javafx
  (:require [muance.javafx-init :as init]
            [muance.core :as m]
            [muance.diff :as diff]
            [muance.arrays :as a]
            [muance.objects :as o]
            [muance.vtree :as vtree]
            [muance.context :as context]
            [muance.attributes :as attributes]
            [muance.animation-timer :as animation-timer]
            [clojure.tools.logging :as log]
            #_[muance.list-cell :as list-cell])
  (:import [java.util ArrayList Collections]
           [javafx.collections ObservableList]
           [javafx.beans.property ObjectProperty]
           [javafx.scene Scene Parent Group]
           [javafx.scene.layout Pane]
           [javafx.scene.control TreeItem]
           [javafx.stage Stage]
           [javafx.beans.property ObjectProperty]
           [java.util.concurrent SynchronousQueue]
           [muance.javafx AnimationTimer FrozenRenderQueue ListCell]))

(defonce stage (init/start-app))

(defonce ^:private typeid (atom 1))

(defn- inc-typeid [t]
  (if (= t (- Long/MAX_VALUE 1)) 1 (inc t)))

(defn- static-symbol? [env s]
  (not (contains? env s)))

(defn- static? [env x]
  (cond (nil? x) true
        (true? x) true
        (false? x) true
        (string? x) true
        (keyword? x) true
        (number? x) true
        (and (seq? x) (= (first x) 'quote)) true
        (vector? x) (every? (partial static? env) x)
        (map? x) (every? (partial static? env) x)
        (symbol? x) (static-symbol? env x)
        :else false))

(declare compile-element-macro)

(defn- compile-form [env form]
  (cond (and (seq? form) (symbol? (first form)))
        (let [clj-var (resolve env (first form))]
          (cond (::tag (meta clj-var))
                (compile-element-macro env
                                       (::tag (meta clj-var))
                                       nil
                                       (::max-children (meta clj-var))
                                       (rest form))
                :else form))
        (string? form) (compile-element-macro env 'javafx.scene.text.Text nil
                                              0 `(:text ~form))
        :else form))

;; Like clojure.string/capitalize but do not lowercase other letters
(defn- ^String capitalize [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1)) (subs s 1)))))

(defn- property-with-prefix [prefix property]
  (->> (name property) capitalize (str prefix)))

(defn- match-property-method? [name params-count ^java.lang.reflect.Method method]
  (and (= (.getName method) name)
       (= params-count (count (.getParameterTypes method)))))

(defn- property-method [tag property-method-name params-count]
  (let [method (->> (.getMethods (Class/forName (str tag)))
                    (filter (partial match-property-method?
                                     property-method-name params-count))
                    first)]
    (when method
      {:name (.getName ^java.lang.reflect.Method method)
       :params (.getParameterTypes ^java.lang.reflect.Method method)})))

(defn seqable->observable-list [s]
  (if (seqable? s)
    (let [o-list (javafx.collections.FXCollections/observableArrayList)]
      (loop [s (seq s)]
        (when-let [f (first s)]
          (.add o-list f)
          (recur (next s))))
      o-list)
    s))

(defn- as-property [tag property]
  (let [property-method-name (str (name property) "Property")]
    (property-method tag property-method-name 0)))

(defn- as-property-setter [tag property]
  (let [property-method-name (property-with-prefix "set" property)]
    (property-method tag property-method-name 1)))

(defn- as-on-property-setter [tag property]
  (let [property-method-name (property-with-prefix "setOn" property)]
    (property-method tag property-method-name 1)))

(defn- format-style-entry [[k v]]
  (if (string? v)
    [(str (name k) ":") (str v) ";"]
    [(str (name k) ":") `(str ~v) ";"]))

(defn- style-map->arr [style-map]
  (mapcat format-style-entry style-map))

(defn- style-arr->calls [style-arr]
  `(doto (java.util.ArrayList.) ~@(for [s style-arr]
                                    `(.add ~s))))

(comment
  (style-map->arr {:display "block"
                   :height 'x
                   :width "33px"})
  (style-map->arr {})

  (style-map->arr {:display nil})
  )

(defn- on-0 [tag key f]
  `(let [f# ~f]
     (when (diff/compare-handlers-0 f#)
       (let [handler# (make-handler-0 f#)]
         (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
            ~(symbol key) handler#)
         (diff/set-handler-0 handler# f#)))
     (diff/inc-attrs 2)))

(defn- on-static-0 [tag key f]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
          ~(symbol key) (make-handler-0 f#)))))

(defn- on-1 [tag key f arg1]
  `(let [f# ~f
         arg1# arg1]
     (when (diff/compare-handlers-1 f# arg1#)
       (let [handler# (make-handler-1 f# arg1#)]
         (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
            ~(symbol key) handler#)
         (diff/set-handler-1 handler# f# arg1#)))
     (diff/inc-attrs 3)))

(defn- on-static-1 [tag key f arg1]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
          ~(symbol key) (make-handler-1 f# ~arg1)))))

(defn- on-2 [tag key f arg1 arg2]
  `(let [f# ~f
         arg1# arg1
         arg2# arg2]
     (when (diff/compare-handlers-2 f# arg1# arg2#)
       (let [handler# (make-handler-2 f# arg1# arg2#)]
         (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
            ~(symbol key) handler#)
         (diff/set-handler-2 handler# f# arg1# arg2#)))
     (diff/inc-attrs 4)))

(defn- on-static-2 [tag key f arg1 arg2]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
          ~(symbol key) (make-handler-2 f# ~arg1 ~arg2)))))

(defn- on-3 [tag key f arg1 arg2 arg3]
  `(let [f# ~f
         arg1# arg1
         arg2# arg2
         arg3# arg3]
     (when (diff/compare-handlers-3 f# arg1# arg2# arg3#)
       (let [handler# (make-handler-3 f# arg1# arg2# arg3#)]
         (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
            ~(symbol key) handler#)
         (diff/set-handler-3 handler# f# arg1# arg2# arg3#)))
     (diff/inc-attrs 5)))

(defn- on-static-3 [tag key f arg1 arg2 arg3]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
          ~(symbol key) (make-handler-3 f# ~arg1 ~arg2 ~arg3)))))

(defn- on-calls [env tag ons]
  (let [static? (partial static? env)
        ons (if (attributes/handler? ons) [ons] ons)]
    (doall
     (for [[k f & args] ons]
       (let [property-name (:name (as-on-property-setter tag k))]
         (assert property-name (str (property-with-prefix "setOn" k) " is not a property of " tag))
         (if (and (static? f) (every? static? args))
           (let [l (count args)]
             (cond (= 0 l) (on-static-0 tag property-name f)
                   (= 1 l) (on-static-1 tag property-name f
                                        (nth args 0))
                   (= 2 l) (on-static-2 tag property-name f
                                        (nth args 0) (nth args 1))
                   :else (on-static-3 tag property-name f
                                      (nth args 0) (nth args 1) (nth args 2))))
           (let [l (count args)]
             (cond (= 0 l) (on-0 tag property-name f)
                   (= 1 l) (on-1 tag property-name f
                                 (nth args 0))
                   (= 2 l) (on-2 tag property-name f
                                 (nth args 0) (nth args 1))
                   :else (on-3 tag property-name f
                               (nth args 0) (nth args 1) (nth args 2))))))))))

(defn- listen-0 [tag key f]
  `(let [f# ~f]
     (when (diff/compare-handlers-0 f#)
       (let [listener# (make-listener-0 f#)]
         (handle-listener
          (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag}) ~(symbol key))
          listener#)
         (diff/set-handler-0 listener# f#)))
     (diff/inc-attrs 2)))

(defn- listen-static-0 [tag key f]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (handle-listener
          (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag}) ~(symbol key))
          (make-listener-0 f#)))))

(defn- listen-1 [key f arg1]
  `(let [f# ~f
         arg1# arg1]
     (when (diff/compare-handlers-1 f# arg1#)
       (diff/set-handler-1
        (. (a/aget diff/*vnode* diff/index-node) ~(symbol key)
           (make-listener-1 f# arg1#))
        f# arg1#))
     (diff/inc-attrs 3)))

(defn- listen-static-1 [key f arg1]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. (a/aget diff/*vnode* diff/index-node) ~(symbol key)
          (make-listener-1 f# ~arg1)))))

(defn- listen-2 [key f arg1 arg2]
  `(let [f# ~f
         arg1# arg1
         arg2# arg2]
     (when (diff/compare-handlers-2 f# arg1# arg2#)
       (diff/set-handler-2
        (. (a/aget diff/*vnode* diff/index-node) ~(symbol key)
           (make-listener-2 f# arg1# arg2#))
        f# arg1# arg2#))
     (diff/inc-attrs 4)))

(defn- listen-static-2 [key f arg1 arg2]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. (a/aget diff/*vnode* diff/index-node) ~(symbol key)
          (make-listener-2 f# ~arg1 ~arg2)))))

(defn- listen-3 [key f arg1 arg2 arg3]
  `(let [f# ~f
         arg1# arg1
         arg2# arg2
         arg3# arg3]
     (when (diff/compare-handlers-3 f# arg1# arg2# arg3#)
       (diff/set-handler-3
        (. (a/aget diff/*vnode* diff/index-node) ~(symbol key)
           (make-listener-3 f# arg1# arg2# arg3#))
        f# arg1# arg2# arg3#))
     (diff/inc-attrs 5)))

(defn- listen-static-3 [key f arg1 arg2 arg3]
  `(let [f# ~f]
     (when (diff/compare-handlers-static f#)
       (. (a/aget diff/*vnode* diff/index-node) ~(symbol key)
          (make-listener-3 f# ~arg1 ~arg2 ~arg3)))))

(defn- listen-calls [env tag listeners]
  (let [static? (partial static? env)
        listeners (if (attributes/handler? listeners) [listeners] listeners)]
    (doall
     (for [[k f & args] listeners]
       (let [property-name (:name (as-property tag k))]
         (assert property-name (str (name k) " is not a property of " tag))
         (if (and (static? f) (every? static? args))
           (let [l (count args)]
             (cond (= 0 l) (listen-static-0 tag property-name f)
                   (= 1 l) (listen-static-1 property-name f (nth args 0))
                   (= 2 l) (listen-static-2 property-name f (nth args 0) (nth args 1))
                   :else (listen-static-3 property-name f
                                          (nth args 0) (nth args 1) (nth args 2))))
           (let [l (count args)]
             (cond (= 0 l) (listen-0 tag property-name f)
                   (= 1 l) (listen-1 property-name f (nth args 0))
                   (= 2 l) (listen-2 property-name f (nth args 0) (nth args 1))
                   :else (listen-3 property-name f
                                   (nth args 0) (nth args 1) (nth args 2))))))))))

(defmulti maybe-cast-param (fn [env property-name param-type property-val]
                             [property-name param-type]))

(defmethod maybe-cast-param ["setItems" javafx.collections.ObservableList]
  [env property-name param-type property-val]
  `(seqable->observable-list ~property-val))

(defmethod maybe-cast-param :default [env property-name param-type property-val]
  property-val)

(defn nil-or-string [v]
  (if (nil? v) nil (str v)))

(defn- prop [key val]
  `(let [val# ~val]
     (when (diff/compare-attrs val#)
       (. (a/aget diff/*vnode* diff/index-node) ~(symbol key) val#)
       (diff/set-attr val#))
     (diff/inc-attrs 1)))

(defn- prop-static [tag key val]
  `(let [val# ~val]
     (when (and (> diff/*new-node* 0) (not (nil? val#)))
       (. ~(with-meta `(a/aget diff/*vnode* diff/index-node) {:tag tag})
          ~(symbol key) val#))))

(defn set-class [c]
  (let [style-class (.getStyleClass ^javafx.scene.Node (a/aget diff/*vnode* diff/index-node))]
    (.clear style-class)
    (.add style-class c)))

(defn set-classes [classes]
  (let [style-class (.getStyleClass ^javafx.scene.Node (a/aget diff/*vnode* diff/index-node))]
    (.clear style-class)
    (doseq [c classes]
      (.add style-class c))))

(defn- style-class [c]
  `(let [c# ~c]
     (when (diff/compare-attrs c#)
       (if (coll? c#)
         (set-classes c#)
         (set-class c#))
       (diff/set-attr c#))
     (diff/inc-attrs 1)))

(defn- style-class-static [c]
  `(let [c# ~c]
     (when (and (> diff/*new-node* 0) (not (nil? c#)))
       (set-class c#))))

(defn- style-classes-static [classes]
  `(let [classes# ~classes]
     (when (and (> diff/*new-node* 0) (not (nil? classes#)))
       (set-classes classes#))))

(defn style-remove-nils! [style-arr i l]
  (when (< i l)
    (let [v (a/aget style-arr (inc i))]
      (when (= (.trim ^String v) "")
        (a/aset style-arr i "")
        (a/aset style-arr (inc i) "")
        (a/aset style-arr (+ i 2) "")))
    (recur style-arr (+ i 3) l)))

(defn join-strings [a]
  (let [sb (StringBuffer.)]
    (a/forEach a #(.append sb %))
    (str sb)))

(defn- style [key val]
  `(let [val# ~val]
     (style-remove-nils! val# 0 (a/length val#))
     (let [val# (join-strings val#)]
       (when (diff/compare-attrs val#)
         (. (a/aget diff/*vnode* diff/index-node) ~(symbol key) val#)
         (diff/set-attr val#)))
     (diff/inc-attrs 1)))

(defn- style-static [key val]
  `(let [val# ~val]
     (style-remove-nils! val# 0 (a/length val#))
     (let [val# (join-strings val#)]
       (when (and (> diff/*new-node* 0) (not (nil? val#)))
         (. (a/aget diff/*vnode* diff/index-node) ~(symbol key) val#)))))

(defn- input-value [tag val]
  (let [node-sym (with-meta (gensym "node") {:tag tag})]
    `(let [val# (nil-or-string ~val)
           ~node-sym (a/aget diff/*vnode* diff/index-node)]
       (when (and (diff/compare-attrs val#)
                  (not= (. ~node-sym ~'getText) val#))
         (. ~node-sym ~'setText val#)
         (diff/set-attr val#))
       (diff/inc-attrs 1))))

(defn- attribute-calls [env tag attrs]
  (reduce (fn [calls [k v]]
            (cond
              (= k ::m/key) calls
              (= k ::m/hooks) calls
              (= k :styleClass) (cond
                                  (and (vector? v) (every? #(static? env %) v))
                                  (conj calls (style-classes-static v))
                                  (static? env v)
                                  (conj calls (style-class-static v))
                                  :else (conj calls (style-class v)))
              (= k :style) (let [style-arr (style-map->arr v)
                                 style-call (style-arr->calls style-arr)
                                 property-name (:name (as-property-setter tag :style))]
                             (assert property-name (str "style is not a property of " tag))
                             (if (every? (partial static? env) style-arr)
                               (conj calls (style-static property-name style-call))
                               (conj calls (style property-name style-call))))
              (= k ::m/on) (into calls (on-calls env tag v))
              (= k ::m/listen) (into calls (listen-calls env tag v))
              (and (isa? (Class/forName (str tag))
                         ;; Dynamically load the class to avoid class loading / javafx init issues
                         (Class/forName "javafx.scene.control.TextField"))
                   (= (name k) "text"))
              (conj calls (if (static? env v)
                            (prop-static tag (:name (as-property-setter tag k)) v)
                            (input-value tag v)))
              :else (let [{property-name :name [param-type] :params} (as-property-setter tag k)]
                      (assert property-name (str (name k) " is not a property of " tag))
                      (conj calls (if (static? env v)
                                    (prop-static
                                     tag
                                     property-name
                                     (maybe-cast-param env property-name param-type v))
                                    (prop
                                     property-name
                                     (maybe-cast-param env property-name param-type v)))))))
          '() attrs))

(defn compile-element-macro
  [env tag typeid max-children body]
  (let [compile-form (partial compile-form env)
        {key ::m/key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update} ::m/hooks
         :as attrs} (attributes/attributes body)
        _ (attributes/validate-attributes attrs)
        body (attributes/body-without-attributes body attrs)]
    (assert (or (nil? max-children) (<= (count body) max-children))
            (str "Too many children for " tag))
    `(do
       (muance.diff/open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
       ~@(attribute-calls env tag attrs)
       ~@(doall (map compile-form body))
       (muance.diff/close ~did-mount ~did-update))))

(defn- method-with-name [method-name ^java.lang.reflect.Method method]
  (= (.getName method) method-name))

(defn- method-name->return-type [^Class c method-name]
  (let [methods (.getMethods c)
        methods (into [] (filter (partial method-with-name method-name)) methods)
        _ (assert (= 1 (count methods)))
        the-method (first methods)]
    (.getReturnType ^java.lang.reflect.Method the-method)))

(defn- get-children->return-type [^Class c]
  (let [methods (.getMethods c)
        methods (into [] (filter (partial method-with-name "getChildren")) methods)
        _ (assert (< (count methods) 2))
        the-method (first methods)]
    (when the-method
      (.getReturnType ^java.lang.reflect.Method the-method))))

(defn emit-context-child-property [tag children-getter]
  `(extend-type ~tag
     context/Context
     (context/insert-before [parent-node# vnode# ref-node#]
       (let [child# (. parent-node# ~children-getter)]
         (.setValue child# (a/aget vnode# diff/index-node))))
     (context/remove-node [parent-node# node#]
       (let [child# (. parent-node# ~children-getter)]
         (when (identical? (.getValue child#) node#)
           (.setValue child# nil))))))

(defn parent-insert-before [^ObservableList children vnode ref-node]
  (let [node (a/aget vnode diff/index-node)]
    ;; javafx forbids duplicate children
    (.remove children node)
    (if (nil? ref-node)
      (.add children node)
      (let [index (.indexOf children ref-node)]
        (if (= index -1)
          (.add children node)
          (.add children index node))))))

(defn emit-context-children-property [tag children-getter]
  `(extend-type ~tag
     context/Context
     (context/insert-before [parent-node# vnode# ref-node#]
       (parent-insert-before (. parent-node# ~children-getter) vnode# ref-node#))
     (context/remove-node [parent-node# node#]
       (.remove (. parent-node# ~children-getter) node#))))

(defn emit-defmacro [name tag max-children]
  `(defmacro ~(vary-meta name assoc
                         ::tag `(quote ~tag)
                         ::max-children max-children)
     [~'& ~'body]
     (swap! @#'typeid #'inc-typeid)
     (compile-element-macro ~'&env (quote ~tag) @@#'typeid ~max-children ~'body)))

(defmacro make-element-macro
  "Defines a new Javafx element macro with the provided tag. The newly defined Javafx element macro
  can be used during a Muance vtree patching to create an Javafx element which name is the provided
  tag."
  [name tag children-getter]
  (let [c (Class/forName (str tag))
        return-type (cond children-getter
                          (method-name->return-type c (str children-getter))
                          (isa? c Parent)
                          (get-children->return-type c))
        max-children (cond (= return-type ObservableList)
                           nil
                           (= return-type ObjectProperty)
                           1
                           :else 0)]
    `(do
       ~(when (and max-children (= return-type ObjectProperty))
          (emit-context-child-property tag children-getter))
       ~(when (and children-getter (= return-type ObservableList))
          (emit-context-children-property tag children-getter))
       ~(emit-defmacro name tag max-children))))

;; Cannot extend parent directly wihtout avoid reflection warnings because the .getChildren method
;; is protected
(extend-protocol context/Context
  Group
  (context/insert-before [parent-node vnode ref-node]
    (parent-insert-before (.getChildren parent-node) vnode ref-node))
  (context/remove-node [parent-node node]
    (.remove (.getChildren parent-node) node))
  Pane
  (context/insert-before [parent-node vnode ref-node]
    (parent-insert-before (.getChildren parent-node) vnode ref-node))
  (context/remove-node [parent-node node]
    (.remove (.getChildren parent-node) node))
  TreeItem
  (context/insert-before [parent-node vnode ref-node]
    (parent-insert-before (.getChildren parent-node) vnode ref-node))
  (context/remove-node [parent-node node]
    (.remove (.getChildren parent-node) node))
  Scene
  (context/insert-before [parent-node vnode ref-node]
    (let [root (.getRoot parent-node)]
      (.setRoot parent-node (a/aget vnode diff/index-node))))
  (context/remove-node [parent-node node]
    ;; Root cannot be nil
    (when (identical? (.getRoot parent-node) node)
      (.setRoot parent-node (Group.)))))

(extend-protocol context/CreateElement
  nil
  (context/create-element [tag] nil)
  Class
  (context/create-element [c]
    (.newInstance c)))

;;;;;;;;;;;;;

(defn run-later-fn [f]
  (javafx.application.Platform/runLater (reify Runnable (run [this] (f)))))

(defmacro run-later [& body]
  `(run-later-fn (fn [] ~@body)))

(deftype JavafxVTree [id vnode render-queue synchronous?]
  vtree/VTree
  (vtree/id [this] id)
  (vtree/vnode [this] vnode)
  (vtree/render-queue [this] render-queue)
  (vtree/synchronous? [this] synchronous?)
  m/VTree
  (m/remove [vtree]
    (let [vnode (vtree/vnode vtree)
          fragment (Group.)]
      (when-let [comp (a/aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* fragment comp nil))
      (a/aset vnode diff/index-node fragment)
      (o/remove diff/roots (vtree/id vtree))))
  (m/refresh [vtree id it]
    (run-later
     (let [vnode (vtree/vnode vtree)
           the-render-queue (vtree/render-queue vtree)
           children (a/aget vnode diff/index-children)]
       (when-let [comp (a/aget children 0)]
         (diff/patch-impl the-render-queue vnode comp
                          (diff/get-comp-render-fn comp)
                          (a/aget comp diff/index-comp-props)
                          true)
         (diff/process-post-render-hooks the-render-queue))))))

(defn- new-root-vnode []
  (doto (ArrayList.)
    (.add nil)
    (.add nil)
    (.add (Group.))
    (.add nil)
    (.add 0)
    (.add (ArrayList.))))

(extend-protocol m/VTreeInsert
  Parent
  (m/insert-before [ref-node vtree]
    (let [parent-node (.getParent ref-node)]
      (let [vnode (vtree/vnode vtree)]
        (when-let [comp (a/aget vnode diff/index-children 0)]
          (diff/insert-vnode-before* parent-node comp ref-node))
        (a/aset vnode diff/index-node parent-node)
        (o/set diff/roots (vtree/id vtree) vtree))))
  (m/append-child [parent-node vtree]
    (let [vnode (vtree/vnode vtree)]
      (when-let [comp (a/aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* parent-node comp nil))
      (a/aset vnode diff/index-node parent-node)
      (o/set diff/roots (vtree/id vtree) vtree)))
  Scene
  (m/insert-before [ref-node vtree]
    (throw (UnsupportedOperationException.)))
  (m/append-child [parent-node vtree]
    (let [vnode (vtree/vnode vtree)]
      (when-let [comp (a/aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* parent-node comp nil))
      (a/aset vnode diff/index-node parent-node)
      (o/set diff/roots (vtree/id vtree) vtree))))

(defn make-handler-0 [f]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.event.EventHandler
        (handle [this e] (f e state-ref))))))

(defn make-handler-1 [f arg1]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.event.EventHandler
        (handle [this e] (f e state-ref arg1))))))

(defn make-handler-2 [f arg1 arg2]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.event.EventHandler
        (handle [this e] (f e state-ref arg1 arg2))))))

(defn make-handler-3 [f state-ref arg1 arg2 arg3]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.event.EventHandler
        (handle [this e] (f e state-ref arg1 arg2 arg3))))))

(defn make-listener-0 [f]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.beans.value.ChangeListener
        (changed [this observalbe o n] (f o n state-ref))))))

(defn make-listener-1 [f arg1]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.beans.value.ChangeListener
        (changed [this observalbe o n] (f o n state-ref arg1))))))

(defn make-listener-2 [f arg1 arg2]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.beans.value.ChangeListener
        (changed [this observalbe o n] (f o n state-ref arg1 arg2))))))

(defn make-listener-3 [f state-ref arg1 arg2 arg3]
  (when (fn? f)
    (let [state-ref diff/*handlers-state-ref*]
      (reify
        javafx.beans.value.ChangeListener
        (changed [this observalbe o n] (f o n state-ref arg1 arg2 arg3))))))

(defn handle-listener [^javafx.beans.value.ObservableValue property
                       ^javafx.beans.value.ChangeListener listener]
  (when diff/*handlers-prev*
    (.removeListener property ^javafx.beans.value.ChangeListener diff/*handlers-prev*))
  (.addListener property listener))

#_(defn comp->list-cell-factory [c]
  (reify javafx.util.Callback
    (call [cb p]
      (let [vt (vtree)]
        (reify javafx.scene.control.ListCell
          (updateItem [list-cell item empty]))))))

(defonce ^:private render-queue-in (SynchronousQueue. true))

;; Returns a frozen render queue and resets the origin render-queue as a side effect
(defn- make-frozen-render-queue [render-queue]
  (let [l (a/length render-queue)
        frozen-queue (ArrayList. ^int l)
        it (.iterator ^ArrayList render-queue)
        components-length (a/length (a/aget render-queue diff/index-render-queue-offset))]
    (loop [i diff/index-render-queue-offset]
      (when (< i l)
        (a/aset frozen-queue i (a/aget render-queue i))
        (recur (inc i))))
    (a/aset render-queue diff/index-render-queue-offset (ArrayList. ^int components-length))
    frozen-queue))

(defn- start-animation-timer [render-queue]
  (.start (AnimationTimer.
           animation-timer/animation-timer-handle
           (FrozenRenderQueue.
            render-queue render-queue-in (SynchronousQueue.)))))

(defn- handle-component-update [in]
  (let [render-queue (a/aget in 0)
        props (a/aget in 1)
        comp-fn (a/aget in 2)
        vnode (a/aget in 3)
        ;; depth == -1 means this was a call to muance.core/patch. In this case, the vnode is
        ;; the vtree vnode. We don't directly pass the vnode of the component at depth 0 because
        ;; it is nil before the firs rendering and this would cause potential concurrency
        ;; (multiple threads) problems
        depth (a/aget in 4)
        comp (if (= depth -1)
               (a/aget vnode diff/index-children 0)
               vnode)
        synchronous? (a/aget in 5)
        processing-flag (a/aget render-queue diff/index-render-queue-processing-flag)
        dirty-flag (a/aget render-queue diff/index-render-queue-dirty-flag)]
    ;; comp == nil means this is the first render
    (if (nil? comp)
      ;; first render is synchronous
      (let [p (promise)]
        (run-later
         (try
           (diff/patch-impl render-queue vnode nil comp-fn props false)
           (diff/process-post-render-hooks render-queue)
           (catch Exception e
             (log/error e))
           (finally (deliver p true))))
        @p)
      (let [comp-data (a/aget comp diff/index-comp-data)]
        ;; if the patch data are coming from a call to the patch fn
        (if (= depth -1) 
          (if-let [dirty-comps (a/aget render-queue diff/index-render-queue-offset)]
            (do
              (a/aset dirty-comps 0 props)
              (a/aset dirty-comps 1 comp-fn)
              (a/aset dirty-comps 2 comp))
            (a/aset render-queue diff/index-render-queue-offset
                    (doto (ArrayList. 3)
                      (.add props)
                      (.add comp-fn)
                      (.add comp))))
          (when-not (identical? dirty-flag (a/aget comp-data diff/index-comp-data-dirty-flag))
            (if-let [dirty-comps (a/aget render-queue (+ depth diff/index-render-queue-offset))]
              (do (a/add dirty-comps props)
                  (a/add dirty-comps comp-fn)
                  (a/add dirty-comps comp))
              (a/aset render-queue (+ depth diff/index-render-queue-offset)
                      (doto (ArrayList.)
                        (.add props)
                        (.add comp-fn)
                        (.add comp))))))
        (a/aset comp-data diff/index-comp-data-dirty-flag dirty-flag)
        (a/aset render-queue diff/index-render-queue-pending-flag true)
        (if synchronous?
          (let [frozen-render-queue (make-frozen-render-queue render-queue)]
            (run-later
             (binding [diff/*rendered-flag* (Object.)]
               (diff/process-render-queue frozen-render-queue)
               ;; process-post-render-hooks with the original queue, not the frozen queue !
               (diff/process-post-render-hooks render-queue))))
          (when-not (a/aget render-queue diff/index-render-queue-processing-flag)
            (a/aset render-queue diff/index-render-queue-processing-flag true)
            (start-animation-timer render-queue)))))))

(defn- handle-animation-timer-request [^FrozenRenderQueue in]
  (let [render-queue (.getOriginRendeQueue in)
        render-queue-out (.getRenderQueueOut in)
        pending-flag (a/aget render-queue diff/index-render-queue-pending-flag)]
    (if pending-flag
      (let [frozen-render-queue (make-frozen-render-queue render-queue)]
        (a/aset render-queue diff/index-render-queue-pending-flag false)
        (a/aset render-queue diff/index-render-queue-dirty-flag (Object.))
        (.put ^SynchronousQueue render-queue-out frozen-render-queue))
      (do
        (a/aset render-queue diff/index-render-queue-processing-flag false)
        (.put ^SynchronousQueue render-queue-out false)))))

(defn- render-queue-worker-impl []
  (let [in (.take ^SynchronousQueue render-queue-in)]
    (if (instance? FrozenRenderQueue in)
      (handle-animation-timer-request in)
      (handle-component-update in))))

(defn- render-queue-fn [in]
  (.put ^SynchronousQueue render-queue-in in))

(defn- render-queue-worker-loop []
  (loop []
    (render-queue-worker-impl)
    (recur)))

(defonce render-queue-worker (doto (Thread. ^Runnable render-queue-worker-loop
                                            "muance-render-queue-worker")
                               (.setDaemon true)
                               (.start)))

(defn- set-post-render-hook [vt post-render-hook]
  (a/aset
   (a/aget (vtree/render-queue vt) diff/index-render-queue-post-render-hooks)
   0 (partial post-render-hook vt)))

(defn vtree
  ([]
   (vtree nil))
  ([{:keys [synchronous? post-render-hook]}]
   (let [vt (->JavafxVTree (swap! diff/vtree-ids inc)
                          (new-root-vnode)
                          ;; render-queue-fn + processing flag + pending flag + dirty-flag
                          ;; + post-render-hooks + render-queue
                          (doto (ArrayList. 6)
                            (.add render-queue-fn)
                            (.add false)
                            (.add false)
                            (.add (Object.))
                            (.add (ArrayList.))
                            (.add (ArrayList.)))
                          synchronous?)]
     (when post-render-hook
       (set-post-render-hook vt post-render-hook))
     vt)))

#_(defn list-cell-factory [comp]
  (reify javafx.util.Callback
    (call [this p]
      (let [state (ArrayList. 2)
            list-cell (ListCell. list-cell/list-cell-updateItem state)
            vt (vtree)]
        (a/aset state list-cell/index-vtree vt)
        (a/aset state list-cell/index-component comp)
        (set-post-render-hook
         vt
         (fn [_]
           (prn (a/aget
                         (vtree/vnode vt)
                         diff/index-children
                         0
                         diff/index-node))
           #_(.setGraphic list-cell
                        (a/aget
                         (vtree/vnode vt)
                         diff/index-children
                         0
                         diff/index-node))))
        list-cell))))
