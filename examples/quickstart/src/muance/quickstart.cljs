(ns muance.quickstart
  (:require
   [muance.core :as m :include-macros true])
  (:require-macros [muance.h :as h]))

(m/defcomp example-component []
  (h/div "my first component"))


(def vtree (m/vtree)) ;; Creates a virtual DOM tree 
(m/append-child vtree (.-body js/document)) ;; Appends the virtual DOM tree to the page
(m/patch vtree example-component) ;; Renders the component
