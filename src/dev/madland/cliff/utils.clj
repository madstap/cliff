(ns dev.madland.cliff.utils)

(defn walk-props
  [command-decl f]
  (letfn [(step [acc [command & [props & more-w-props :as more-no-props]]]
            (let [props? (map? props)
                  new-props (if (= ::nothing acc)
                              (if props? props {})
                              (f acc (when props? props)))]
              (into [command new-props]
                    (map (partial step new-props))
                    (if props? more-w-props more-no-props))))]
    (step ::nothing command-decl)))

(defn map-props
  [command-decl f]
  (letfn [(step [[command & [props & more-w-props :as more-no-props]]]
            (let [props? (map? props)
                  new-props (f (if props? props {}))]
              (into [command new-props]
                    (map step)
                    (if props? more-w-props more-no-props))))]
    (step command-decl)))


(comment

  ;; TODO: Tests
  (map-props
   ["foo" {:x 1}
    ["bar" {:x 2}]]
   #(assoc % :foo 123))

  (walk-props
   ["foo" {:x 1}
    ["bar" {:y 2}]]
   merge)
  )

(defn update-existing [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn map-kvs [f m]
  (into {} (map f) m))

(defn conj-some [coll & xs]
  (apply conj coll (remove nil? xs)))

(defn assoc-some
  ([ m k v]
   (cond-> m (some? v) (assoc k v)))
  ([m k v & kvs]
   (reduce (partial apply assoc-some) (assoc-some m k v) (partition 2 kvs))))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn map-kv-vals [f m]
  (into {} (map (fn [[k v]] [k (f k v)])) m))

(defn index-by [f coll]
  (into {} (map (juxt f identity)) coll))
