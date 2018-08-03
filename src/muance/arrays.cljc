(ns muance.arrays
  (:refer-clojure :exclude [aset aget pop object-array])
  #?(:clj (:import [java.util ArrayList])))

#?(:clj
   (defn object-array
     ([x1]
      (doto (clojure.core/object-array 1)
        (clojure.core/aset 0 x1)))
     ([x1 x2]
      (doto (clojure.core/object-array 2)
        (clojure.core/aset 0 x1)
        (clojure.core/aset 1 x2)))
     ([x1 x2 x3]
      (doto (clojure.core/object-array 3)
        (clojure.core/aset 0 x1)
        (clojure.core/aset 1 x2)
        (clojure.core/aset 2 x3)))))

#?(:clj
   (defn aset [^ArrayList a i v]
     (let [add-nb (inc (- i (.size a)))]
       (dotimes [i add-nb]
         (.add a nil)))
     (.set a i v)))

#?(:cljs
   (def aset cljs.core/aset))

#?(:clj
   (defn aget
     ([^ArrayList a i]
      (when (< i (.size a))
        (.get a i)))
     ([^ArrayList a i i2]
      (aget (aget a i) i2))
     ([^ArrayList a i i2 i3]
      (aget (aget a i) i2 i3))))

#?(:cljs
   (def aget cljs.core/aget))

#?(:clj
   (defn add [^ArrayList a e]
     (.add a e)))

#?(:cljs
   (defn add [a e]
     (.push a e)))

#?(:clj
   (defn pop [^ArrayList a]
     (let [s (.size a)]
       (when (> s 0)
         (.remove a (dec s))))))

#?(:cljs
   (defn pop [a]
     (.pop a)))

#?(:cljs
   (defn length [a]
     (.-length a)))

#?(:clj
   (defn length [^ArrayList a]
     (.size a)))

#?(:cljs
   (defn forEach [a f]
     (.forEach a f)))

#?(:clj
   (defn forEach [^ArrayList a f]
     (let [it (.iterator a)]
       (while (.hasNext it)
         (f (.next it))))))

(comment
  (def aa (ArrayList.))
  (aset aa 1 33)
  aa
  (pop aa)
  (aget aa 0)
  )

(comment
  #?(:cljs
     (pop #js [1 2]))
  )
