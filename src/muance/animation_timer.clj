(ns muance.animation-timer
  (:require [muance.diff :as diff]))

(deftype FrozenRenderQueue [origin-render-queue
                            render-queue-in
                            ;; A synchronous queue of frozen render queues
                            render-queue-out])

(gen-class
 :name "muance.javafx.AnimationTimer"
 :extends javafx.animation.AnimationTimer
 :prefix "animation-timer-"
 :state "frozen_render_queue"
 :constructors {[muance.animation_timer.FrozenRenderQueue] []}
 :init "init")

(defn- animation-timer-init [frozen-render-queue]
  [[] frozen-render-queue])

(defn- animation-timer-handle [^muance.javafx.AnimationTimer this t]
  (let [^FrozenRenderQueue frozen-render-queue (.frozen-render-queue this)
        origin-render-queue (.origin-render-queue frozen-render-queue)
        render-queue-in (.render-queue-in frozen-render-queue)
        render-queue-out (.render-queue-out frozen-render-queue)]
    (loop []
      ;; Query from the pending render-queue
      (.put ^java.util.concurrent.SynchronousQueue render-queue-in frozen-render-queue)
      ;; We get false when there is no more components to render
      (let [render-queue (.take ^java.util.concurrent.SynchronousQueue render-queue-out)]
        (when render-queue
          (binding [diff/*rendered-flag* (Object.)]
            (diff/process-render-queue render-queue)
            ;; process-post-render-hooks with the origin queue, not the frozen queue !
            (diff/process-post-render-hooks origin-render-queue))
          (recur))))
    (.stop this)))

;; Reloading this namespace may require restarting the REPL and AOT compiling it again. This is why it is put apart (to avoid classloading/AOT issues)
