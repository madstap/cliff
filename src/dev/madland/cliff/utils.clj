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

(defn update-existing [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn map-kvs [f m]
  (into {} (map f) m))

(defn conj-some [coll & xs]
  (apply conj coll (remove nil? xs)))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn map-kv-vals [f m]
  (into {} (map (fn [[k v]] [k (f k v)])) m))

(defn index-by [f coll]
  (into {} (map (juxt f identity)) coll))
