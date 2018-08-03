(ns muance.javafx
  (:require [muance.core :as m]
            [muance.diff :as diff]
            [muance.arrays :as a]
            [muance.objects :as o]
            [muance.vtree :as vtree]
            [muance.context :as context])
  (:import [java.util ArrayList]
           [javafx.collections ObservableList]
           [javafx.beans.property ObjectProperty]
           [javafx.scene Scene Parent Group]
           [javafx.stage Stage]
           [javafx.beans.property ObjectProperty]))

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

(defn- attributes [body]
  nil)

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
        (string? form) (compile-element-macro env "javafx.scene.text.Text" nil
                                              0 `(:text ~form))
        :else form))

(defn- attribute-calls [env tag attrs]
  nil)

(defn- handler? [h]
  (and (vector? h) (keyword? (first h))))

(defn- validate-attributes [{:keys [::m/hooks style ::m/on] :as attributes}]
  (when (contains? attributes ::m/hooks) (assert (map? hooks)))
  (when (contains? attributes :style) (assert (map? style)))
  (when (contains? attributes ::m/on) (assert (or (handler? on) (every? handler? on)))))

(defn- body-without-attributes [body attributes]
  (drop (* 2 (count attributes)) body))

#_(defn compile-element-macro
  [env tag typeid children-getter max-children body]
  (let [compile-form (partial compile-form env)
        {key ::m/key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update} ::m/hooks :as attrs} (attributes body)
        _ (validate-attributes attrs)
        body (body-without-attributes body attrs)]
    (assert (or (nil? max-children) (<= (count body) max-children))
            (str "Too many children for " tag))
    (if children-getter
      `(let [prev-children# diff/*children*]
         (muance.diff/open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
         (set! diff/*children*
               (~'.
                (a/aget diff/*vnode* diff/index-node)
                ~children-getter))
         ~@(attribute-calls env tag attrs)
         ~@(map compile-form body)
         (muance.diff/close ~did-mount ~did-update)
         (set! diff/*children* prev-children#))
      `(do
         (muance.diff/open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
         ~@(attribute-calls env tag attrs)
         ~@(map compile-form body)
         (muance.diff/close ~did-mount ~did-update)))))

(defn compile-element-macro
  [env tag typeid max-children body]
  (let [compile-form (partial compile-form env)
        {key ::m/key
         {will-update :will-update will-unmount :will-unmount
          remove-hook :remove-hook
          did-mount :did-mount did-update :did-update} ::m/hooks :as attrs} (attributes body)
        _ (validate-attributes attrs)
        body (body-without-attributes body attrs)]
    (assert (or (nil? max-children) (<= (count body) max-children))
            (str "Too many children for " tag))
    `(do
       (muance.diff/open ~tag ~typeid ~key ~will-update ~will-unmount ~remove-hook)
       ~@(attribute-calls env tag attrs)
       ~@(map compile-form body)
       (muance.diff/close ~did-mount ~did-update))))

#_(defmacro make-element-macro
  "Defines a new Javafx element macro with the provided tag. The newly defined Javafx element macro
  can be used during a Muance vtree patching to create an Javafx element which name is the provided
  tag."
  [name tag children-getter max-children]
  `(defmacro ~(vary-meta name assoc
                         ::tag (str tag)
                         ::children-getter `(quote ~children-getter)
                         ::max-children max-children)
     [~'& ~'body]
     (swap! @#'typeid #'inc-typeid)
     (compile-element-macro ~'&env ~(str tag) @@#'typeid
                            (quote ~children-getter)
                            ~max-children ~'body)))

(defn- method-with-name [method-name method]
  (= (.getName method) method-name))

(defn- method-name->return-type [c method-name]
  (let [methods (.getMethods c)
        methods (into [] (filter (partial method-with-name method-name)) methods)
        _ (assert (= 1 (count methods)))
        the-method (first methods)]
    (.getReturnType the-method)))

(defn- get-children->return-type [c]
  (let [methods (.getMethods c)
        methods (into [] (filter (partial method-with-name "getChildren")) methods)
        _ (assert (< (count methods) 2))
        the-method (first methods)]
    (when the-method
      (.getReturnType the-method))))

(defn emit-context-child-property [tag children-getter]
  `(extend-type ~tag
     context/Context
     (context/insert-before [parent-node# vnode# ref-node#]
       (let [child# (. parent-node# ~children-getter)]
         (.setValue child# (a/aget vnode# diff/index-node))))
     (context/remove-node [parent-node# node#]
       (let [child# (. parent-node# ~children-getter)]
         (.setValue child# nil)))))

(defn emit-defmacro [name tag max-children]
  `(defmacro ~(vary-meta name assoc
                           ::tag (str tag)
                           ::max-children max-children)
       [~'& ~'body]
       (swap! @#'typeid #'inc-typeid)
       (compile-element-macro ~'&env ~(str tag) @@#'typeid
                              ~max-children ~'body)))

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
       ~(emit-defmacro name tag max-children))))

(defn remove-from-children [children node]
  )

(extend-protocol context/Context
  Parent
  (context/insert-before [parent-node vnode ref-node]
    (let [^ObservableList children (.getChildren parent-node)
          node (a/aget vnode diff/index-node)]
      ;; javafx forbids duplicate children
      (.remove children node)
      (if (nil? ref-node)
        (.add children node)
        (let [index (.indexOf children ref-node)]
          (if (= index -1)
            (.add children node)
            (.add children index node))))))
  (context/remove-node [parent-node node]
    (let [^ObservableList children (.getChildren parent-node)]
      (.remove children node)))
  Scene
  (context/insert-before [parent-node vnode ref-node]
    (let [root (.getRoot parent-node)]
      (.setRoot parent-node (a/aget vnode diff/index-node))))
  (context/remove-node [parent-node node]
    ;; Root cannot be nil
    (.setRoot parent-node (Group.))))

(extend-protocol context/CreateElement
  nil
  (context/create-element [tag] nil)
  Class
  (context/create-element [c]
    (.newInstance c))
  String
  (context/create-element [tag]
    (.newInstance (Class/forName tag))))

;;;;;;;;;;;;;

(defn run-later-fn [f]
  (javafx.application.Platform/runLater (reify Runnable (run [this] (f)))))

(defmacro run-later [& body]
  `(run-later-fn (fn [] ~@body)))

(deftype JavafxVTree [id vnode render-queue]
  vtree/VTree
  (vtree/id [this] id)
  (vtree/vnode [this] vnode)
  (vtree/render-queue [this] render-queue)
  (vtree/synchronous-first-render [this] false)
  m/VTree
  (m/remove [vtree]
    (let [vnode (vtree/vnode vtree)
          fragment (Group.)]
      (when-let [comp (a/aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* fragment comp nil))
      (a/aset vnode diff/index-node fragment)
      (o/remove diff/roots (vtree/id vtree)))))

(defn- new-root-vnode []
  (doto (ArrayList.)
    (.add nil)
    (.add nil)
    (.add (Group.))
    (.add nil)
    (.add 0)
    (.add (ArrayList.))))

(defn vtree []
  (->JavafxVTree (swap! diff/vtree-ids inc)
                 (new-root-vnode)
                 ;; async-fn + post render hooks + internal post render hooks
                 (doto (ArrayList.)
                   (.add run-later-fn)
                   (.add (ArrayList.))
                   (.add (ArrayList.)))))

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
      (prn (a/aget vnode diff/index-children 0))
      (when-let [comp (a/aget vnode diff/index-children 0)]
        (diff/insert-vnode-before* parent-node comp nil))
      (a/aset vnode diff/index-node parent-node)
      (o/set diff/roots (vtree/id vtree) vtree))))
