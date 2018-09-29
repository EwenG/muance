(ns muance.local-state
  (:refer-clojure :exclude [atom])
  (:require [muance.context :as context]))

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
    muance-watcher))

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
