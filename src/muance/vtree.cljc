(ns muance.vtree)

;; id is used to keep track of rendered vtrees, for re-rendering on function/component reload
(defprotocol VTree
  (id [this])
  (vnode [this])
  (render-queue [this]))
