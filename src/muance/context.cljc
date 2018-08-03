(ns muance.context)

(defprotocol Context
  (insert-before [parent-node vnode ref-node])
  (remove-node [parent-node node]))

(defprotocol CreateElement
  (create-element [tag]))

(defprotocol ILocalState
  (-remove-muance-watcher [this]))
