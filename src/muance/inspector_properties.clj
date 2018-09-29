(ns muance.inspector-properties
  (:require [muance.core :as m]
            [muance.j :as j]
            [muance.javafx :as javafx]
            [clojure.java.io :as io])
  (:import [javafx.css StyleOrigin PseudoClass SimpleSelector CompoundSelector ParsedValue]
           [javafx.scene Node]
           [javafx.scene.control ScrollPane ScrollPane$ScrollBarPolicy TableColumn TableRow]
           [javafx.scene.layout BorderPane GridPane]
           [javafx.geometry Pos]
           [java.net MalformedURLException URISyntaxException URI URL]
           [java.lang.reflect InvocationTargetException]
           [java.security DigestInputStream MessageDigest NoSuchAlgorithmException]
           [java.io IOException File]
           [java.util Collections]
           [java.nio.file Paths]
           [java.awt Desktop]))

(defn make-path [s & ss]
  (Paths/get s (into-array String ss)))

(defn selector-name [sel]
  (cond (instance? CompoundSelector sel)
        (clojure.string/join ", " (.getSelectors sel))
        :else (str sel)))

(defn format-stylesheet-url [url]
  (let [url (URL. url)]
    (case (.getProtocol url)
      "file" (let [path (-> url (.getPath) make-path)
                   user-dir (System/getProperty "user.dir")]
               (if (.startsWith path user-dir)
                 (str (.relativize (make-path user-dir) path))
                 (str (.getFileName path))))
      "jar" (-> url (.openConnection) (.getEntryName) make-path (.getFileName) str)
      (str url))))

(defn make-stylesheet-link [url]
  (let [url (URL. url)]
    (case (.getProtocol url)
      "file" (let [f (File. (.getPath url))]
               (when (.exists f) f))
      "jar" (let [jar-file (-> url (.openConnection) (.getJarFileURL)
                               (.getPath) make-path (.toFile))]
              (when (.exists jar-file)
                jar-file))
      nil)))

(defmulti style-pane-head (fn [selector] (.getOrigin (.getRule selector))))

(defmethod style-pane-head StyleOrigin/INLINE
  [selector]
  (j/text :text "Inline styles"))

(defmethod style-pane-head StyleOrigin/AUTHOR
  [selector]
  (j/text :text (selector-name selector)))

(defmethod style-pane-head StyleOrigin/USER_AGENT
  [selector]
  (j/text :text (selector-name selector)))

(defmethod style-pane-head StyleOrigin/USER
  [selector]
  (j/text :text "User styles"))

(defn show-stylesheet [e state-ref f]
  ;; Calling open on the javafx threads fails
  (future (.open (Desktop/getDesktop) f)))

(defn format-parsed-value [value]
  (cond
    (nil? value) "nil"
    (instance? ParsedValue value)
    (if-let [converter (.getConverter value)]
      (str converter "(" (format-parsed-value (.getValue value)) ")")
      (format-parsed-value (.getValue value)))
    (and value (.isArray (class value)))
    (clojure.string/join ", " (map format-parsed-value value))
    :else (str value)))

(defn style-selectors [selected-item]
  (let [node (:node (.getValue selected-item))
        selectors (.findMatchingStyles (muance.javafx.StyleManager/getInstance) node)
        _ (Collections/reverse selectors)
        cascading-styles (.getCascadingStyles (com.sun.javafx.css.StyleMap. 0 (.clone selectors)))]
    (doseq [selector selectors
            :let [rule (.getRule selector)]]
      (j/border-pane
       ::m/hooks {:did-mount (fn [selected-item state]
                               (.bind (.prefWidthProperty (m/node))
                                      (.widthProperty (m/parent-node))))}
       :style {:-fx-background-color "white"}
       (when-let [stylesheet (.getStylesheet rule)]
         (when-let [url (.getUrl stylesheet)]
           (if-let [link (make-stylesheet-link url)]
             (j/hyperlink
              ::m/hooks {:did-mount (fn [selected-item state]
                                      (javafx/set-position-in-border-pane-top (m/node))
                                      (BorderPane/setAlignment (m/node) Pos/TOP_RIGHT))}
              ::m/on [:action show-stylesheet link]
              :text (format-stylesheet-url url))
             (j/text :text (format-stylesheet-url url)))))
       (j/text-flow
        ::m/hooks {:did-mount (fn [selected-item state]
                                (javafx/set-position-in-border-pane-center (m/node)))}
        (style-pane-head selector)
        (j/text :text " {\n")
        (doseq [declaration (.getDeclarations rule)
                :let [property-k (.getProperty declaration)
                      property-v (format-parsed-value (.getParsedValue declaration))]]
          (j/text :text "  ")
          (j/text :text property-k)
          (j/text :text ": ")
          (j/text :text property-v)
          (j/text :text "\n"))
        (j/text :text "}"))))))

(m/defcomp cell-comp [x]
  (j/h-box
   (j/text :text "ee")))

#_(defn style-selectors [selected-item]
  (let [node (:node (.getValue selected-item))
        selectors (.findMatchingStyles (muance.javafx.StyleManager/getInstance) node)
        _ (Collections/reverse selectors)
        cascading-styles (.getCascadingStyles (com.sun.javafx.css.StyleMap. 0 (.clone selectors)))]
    (doseq [selector selectors
            :let [rule (.getRule selector)]]
      (j/grid-pane
       (j/stack-pane
        :style {:-fx-border-color "-fx-box-border"
                :-fx-background-color "-fx-inner-border, -fx-body-color"
                :-fx-background-insets "0, 1"}
        (j/text :text "Inline-style"))
       (j/group
        ::m/hooks {:did-mount (fn [props state]
                                (GridPane/setColumnIndex (m/node) (int 1)))}
        (j/hyperlink :text "css link"))
       (j/group
        ::m/hooks {:did-mount (fn [props state]
                                (GridPane/setRowIndex (m/node) (int 1)))}
        (j/text :text "prop k"))
       (j/group
        ::m/hooks {:did-mount (fn [props state]
                                (GridPane/setColumnIndex (m/node) (int 1))
                                (GridPane/setRowIndex (m/node) (int 1)))}
        (j/text :text "prop v"))))))

(m/defcomp properties-pane
  ::m/hooks {:did-mount (fn [selected-item state]
                          (def nn (:node (.getValue selected-item))))
             #_:will-receive-props #_(fn [selected-item state]
                                   (let [prev-selected-item (m/get :selected-item)]
                                     (when-not (identical? prev-selected-item selected-item)
                                       (remove-stylesheet-listeners prev-selected-item))))}
  [selected-item]
  #_(j/table-view
   :items ["e" "f"]
   (j/table-column
    ::m/hooks {:did-mount (fn [props state]
                            (.setCellFactory (m/node) (javafx/table-cell-factory cell-comp)))})
   (j/table-column
    ::m/hooks {:did-mount (fn [props state]
                            (.setCellFactory (m/node) (javafx/table-cell-factory cell-comp)))})
   )

  
  #_(j/grid-pane
   #_#_:gridLinesVisible true
   (j/stack-pane
    :style {:-fx-border-color "-fx-box-border"
            :-fx-background-color "-fx-inner-border, -fx-body-color"
            :-fx-background-insets "0, 1"}
    (j/text :text "Inline-style"))
   (j/group
    ::m/hooks {:did-mount (fn [props state]
                            (GridPane/setColumnIndex (m/node) (int 1)))}
    (j/hyperlink :text "css link"))
   (j/group
    ::m/hooks {:did-mount (fn [props state]
                            (GridPane/setRowIndex (m/node) (int 1)))}
    (j/text :text "prop k"))
   (j/group
    ::m/hooks {:did-mount (fn [props state]
                            (GridPane/setColumnIndex (m/node) (int 1))
                            (GridPane/setRowIndex (m/node) (int 1)))}
    (j/text :text "prop v")))

  
  (j/tab-pane
   :styleClass "styles-properties-pane"
   (let [styleable (:node (.getValue selected-item))]
     (when (instance? javafx.css.Styleable styleable)
       (j/tab
        :text "Styles"
        :closable false
        (j/scroll-pane
         (j/v-box
          ::m/hooks {:did-mount (fn [selected-item state]
                                  (.bind (.maxWidthProperty (m/node))
                                         (.subtract (.widthProperty (m/parent-node)) 20))
                                  (.bind (.minWidthProperty (m/node))
                                         (.subtract (.widthProperty (m/parent-node)) 20)))}
          (style-selectors selected-item))))))
   (j/tab :text "Properties"
          :closable false)))

(comment
  (.getStylesheets nn)
  (map #(.getProperty %) (.getCssMetaData nn))
  (first (.getCssMetaData nn))
  (.getProperty (first (.getCssMetaData nn)))
  (.getInitialValue (first (.getCssMetaData nn)) nn)
  (.getValue (.getStyleableProperty (first (.getCssMetaData nn)) nn))
  (.getStyleOrigin (.getStyleableProperty (second (.getCssMetaData nn)) nn))
  )

;; LinearGradientConverter --- 
;; RadialGradientConverter --- 
;; PaintConverter.SequenceConverter --- ... , ...
