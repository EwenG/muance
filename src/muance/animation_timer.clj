(ns muance.animation-timer
  (:require [muance.diff :as diff]
            [clojure.tools.logging :as log])
  (:import [muance.javafx AnimationTimer FrozenRenderQueue]))

(defn animation-timer-handle [^AnimationTimer this t]
  (let [frozen-render-queue (.getFrozenRenderQueue this)
        origin-render-queue (.getOriginRendeQueue frozen-render-queue)
        render-queue-in (.getRenderQueueIn frozen-render-queue)
        render-queue-out (.getRenderQueueOut frozen-render-queue)]
    (loop []
      ;; Query from the pending render-queue
      (.put render-queue-in frozen-render-queue)
      ;; We get false when there is no more components to render
      (let [render-queue (.take render-queue-out)]
        (when render-queue
          (try
            (binding [diff/*rendered-flag* (Object.)]
              (diff/process-render-queue render-queue)
              ;; process-post-render-hooks with the origin queue, not the frozen queue !
              (diff/process-post-render-hooks origin-render-queue))
            (catch Exception e
              (log/error e)))
          (recur))))
    (.stop this)))

;; Reloading this namespace may require restarting the REPL and AOT compiling it again. This is why it is put apart (to avoid classloading/AOT issues)
