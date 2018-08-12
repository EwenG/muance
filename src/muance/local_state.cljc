(ns muance.local-state
  (:refer-clojure :exclude [atom])
  (:require [muance.context :as context])
  #?(:clj (:import [java.util.concurrent.atomic AtomicReference])))

#?(:cljs
   (deftype LocalStateAtom [^:mutable state watches ^:mutable muance-watcher component-data]
     Object
     (equiv [this other]
       (-equiv this other))

     IAtom

     IEquiv
     (-equiv [o other] (identical? o other))

     IDeref
     (-deref [_] state)

     IWatchable
     (-notify-watches [this oldval newval]
       (when muance-watcher
         (muance-watcher component-data newval))
       (when-not (nil? watches)
         (doseq [[key f] watches]
           (f key this oldval newval))))
     (-add-watch [this key f]
       (set! (.-watches this) (assoc watches key f))
       this)
     (-remove-watch [this key]
       (set! (.-watches this) (dissoc watches key)))

     IReset
     (-reset! [this new-value]
       (let [old-value state]
         (set! state new-value)
         (-notify-watches this old-value new-value)
         new-value))

     ISwap
     (-swap! [this f]
       (-reset! this (f state)))
     (-swap! [this f a]
       (-reset! this (f state a)))
     (-swap! [this f a b]
       (-reset! this (f state a b)))
     (-swap! [this f a b xs]
       (-reset! this (apply f state a b xs)))

     IHash
     (-hash [this] (goog/getUid this))

     context/ILocalState
     (context/-remove-muance-watcher [this]
       (set! muance-watcher nil))
     (context/-muance-watcher [this]
       muance-watcher)))

#?(:clj
   (declare notify-watches))

#?(:clj
   (deftype LocalStateAtom [^AtomicReference state
                            ^{:volatile-mutable true
                              :tag clojure.lang.IPersistentMap} watches
                            ^{:volatile-mutable true} muance-watcher
                            component-data]
     clojure.lang.IDeref
     (deref [this] (.get state))
     clojure.lang.IRef
     (getWatches [this]
       watches)
     (addWatch [this key callback]
       (set! watches (.assoc watches key callback))
       this)
     (removeWatch [this key]
       (set! watches (.without watches key))
       this)
     clojure.lang.IAtom2
     (swap [this f]
       (loop []
         (let [v (deref this)
               new-v (f v)]
           (if (.compareAndSet state v new-v)
             (do
               (notify-watches this v new-v)
               new-v)
             (recur)))))
     (swap [this f arg]
       (loop []
         (let [v (deref this)
               new-v (f v arg)]
           (if (.compareAndSet state v new-v)
             (do
               (notify-watches this v new-v)
               new-v)
             (recur)))))
     (swap [this f arg1 arg2]
       (loop []
         (let [v (deref this)
               new-v (f v arg1 arg2)]
           (if (.compareAndSet state v new-v)
             (do
               (notify-watches this v new-v)
               new-v)
             (recur)))))
     (swap [this f x y args]
       (loop []
         (let [v (deref this)
               new-v (apply f v x y args)]
           (if (.compareAndSet state v new-v)
             (do
               (notify-watches this v new-v)
               new-v)
             (recur)))))
     (swapVals [this f]
       (loop []
         (let [old-v (deref this)
               new-v (f old-v)]
           (if (.compareAndSet state old-v new-v)
             (do
               (notify-watches this old-v new-v)
               [old-v new-v])
             (recur)))))
     (swapVals [this f arg]
       (loop []
         (let [old-v (deref this)
               new-v (f old-v arg)]
           (if (.compareAndSet state old-v new-v)
             (do
               (notify-watches this old-v new-v)
               [old-v new-v])
             (recur)))))
     (swapVals [this f arg1 arg2]
       (loop []
         (let [old-v (deref this)
               new-v (f old-v arg1 arg2)]
           (if (.compareAndSet state old-v new-v)
             (do
               (notify-watches this old-v new-v)
               [old-v new-v])
             (recur)))))
     (swapVals [this f x y args]
       (loop []
         (let [old-v (deref this)
               new-v (apply f x y args)]
           (if (.compareAndSet state old-v new-v)
             (do
               (notify-watches this old-v new-v)
               [old-v new-v])
             (recur)))))
     (compareAndSet [this oldv newv]
       (let [ret (.compareAndSet state oldv newv)]
         (when ret
           (notify-watches this oldv newv))
         ret))
     (reset [this newval]
       (let [oldval (.get state)]
         (.set state newval)
         (notify-watches this oldval newval)
         newval))
     (resetVals [this newv]
       (loop []
         (let [oldv (deref this)]
           (if (.compareAndSet state oldv newv)
             (do
               (notify-watches this oldv newv)
               [oldv newv])
             (recur)))))

     context/ILocalState
     (context/-remove-muance-watcher [this]
       (set! muance-watcher nil))
     (context/-muance-watcher [this]
       muance-watcher)))

#?(:clj
   (defn notify-watches [^LocalStateAtom a old-val new-val]
     (let [muance-watcher (context/-muance-watcher a)
           ws (.getWatches a)]
       (when muance-watcher
         (muance-watcher (.-component-data a) new-val))
       (if (> (.count ws) 0)
         (doseq [[k f] ws]
           (when f
             (f k a old-val new-val)))))))

(comment
  (def aaa (->LocalStateAtom (AtomicReference.)
                             (clojure.lang.PersistentHashMap/EMPTY)
                             (fn [comp-data new-v]
                               (prn "muance watcher" comp-data new-v))
                             (doto (java.util.ArrayList.) (.add 1))))

  (reset! aaa "ee")
  (swap! aaa (constantly "gg3") 33 44 55 66)
  (swap-vals! aaa (constantly "vv7") 44 55 "e" 77)
  (reset-vals! aaa "rr2")
  (compare-and-set! aaa "rr2" "rr3")
  (add-watch aaa ::watch (fn [k r o n] (prn "watch" n)))
  (remove-watch aaa ::watch)
  )


;; Cljs has no protocol for -reset-vals! .......
;; Implement meta and validators for LocalStateAtom
