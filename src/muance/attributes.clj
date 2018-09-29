(ns muance.attributes)

(defn handler? [h]
  (and (vector? h) (keyword? (first h))))

(defn validate-attributes
  [{:keys [:muance.core/hooks style :muance.core/on] :as attributes}]
  (when (contains? attributes :muance.core/hooks) (assert (map? hooks)))
  (when (contains? attributes :style) (assert (map? style)))
  (when (contains? attributes :muance.core/on)
    (assert (or (handler? on) (every? handler? on)))))

(defn body-without-attributes [body attributes]
  (drop (* 2 (count attributes)) body))

(def ^{:private true} props-to-rename
  {:class :className
   :for :htmlFor
   :accept-charset :acceptCharset
   :http-equiv :httpEquiv})

(defn- rename-prop [[k v :as prop]]
  (if-let [new-k (get props-to-rename k)]
    [new-k v]
    prop))

(defn attributes [body]
  (let [attrs (->> (partition 2 body)
                   (take-while (comp keyword? first))
                   (map rename-prop))]
    (when (not (empty? attrs))
      (let [attrs-keys (map first attrs)]
        (assert (apply distinct? attrs-keys)
                (str "duplicate attributes: " (pr-str attrs-keys)))
        (let [attrs-keys (remove #(or (= :muance.core/hooks %)
                                      (= :muance.core/key %)
                                      (= :muance.core/on %))
                                 attrs-keys)]
          (when (not (empty? attrs-keys))
            (assert (apply distinct? (map name attrs-keys))
                    (str "duplicate attributes: " (pr-str attrs-keys)))))))
    (into {} (map vec attrs))))

(defn map-attributes [f body]
  (let [attrs (->> (partition 2 body)
                   (take-while (comp keyword? first))
                   (mapcat f))
        body-rest (drop (count attrs) body)]
    (concat attrs body-rest)))
