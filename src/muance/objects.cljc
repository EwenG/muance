(ns muance.objects
  (:refer-clojure :exclude [get remove set])
  #?(:cljs (:require [goog.object :as o])))

#?(:cljs
   (defn get [o k]
     (o/get o k)))

#?(:clj
   (defn get [^java.util.HashMap o k]
     (.getOrDefault o k nil)))

#?(:cljs
   (defn set [o k v]
     (o/set o k v)))

#?(:clj
   (defn set [^java.util.HashMap o k v]
     (.put o k v)))

#?(:cljs
   (defn remove [o k]
     (o/remove o k)))

#?(:clj
   (defn remove [^java.util.HashMap o k]
     (.remove o k)))

#?(:cljs
   (defn forEach [o f]
     (o/forEach o f)))

#?(:clj
   (defn forEach [^java.util.HashMap o f]
     (let [it (.iterator (.entrySet o))]
       (while (.hasNext it)
         (let [^java.util.Map$Entry entry (.next it)]
           (f (.getValue entry) (.getKey entry) it))))))

#?(:cljs
   (defn forEachRemove [o k]
     (o/remove o k)))

#?(:clj
   (defn forEachRemove [^java.util.Iterator it]
     (.remove it)))

(comment
  (def mmm (doto (java.util.HashMap.)
             (.put "k" "v")
             (.put "k2" "v2")))

  (forEach mmm (fn [v k it]
                 (prn v k)
                 (when (= v "v")
                   (forEachRemove it))))
  
  )
