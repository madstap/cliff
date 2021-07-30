(ns dev.madland.cliff.utils
  (:require [dev.madland.cliff.vendor.tools-cli :as cli*]
            [clojure.string :as str]))

(defn walk-props
  [cli f]
  (letfn [(step [acc [command & [props & more-w-props :as more-no-props]]]
            (let [props? (map? props)
                  new-props (if (= ::nothing acc)
                              (if props? props {})
                              (f acc (when props? props)))]
              (into [command new-props]
                    (map (partial step new-props))
                    (if props? more-w-props more-no-props))))]
    (step ::nothing cli)))

(defn map-props
  [cli f]
  (letfn [(step [[command & [props & more-w-props :as more-no-props]]]
            (let [props? (map? props)
                  new-props (f (if props? props {}))]
              (into [command new-props]
                    (map step)
                    (if props? more-w-props more-no-props))))]
    (step cli)))

(defn update-props
  [cli commands f & args]
  (letfn [(step [current-commands [command & [props & more-w-props :as more-no-props]]]
            (let [props? (map? props)
                  new-commands (conj current-commands command)
                  update? (= new-commands commands)
                  new-props (if update?
                              (apply f (if props? props {}) args)
                              (if props? props {}))]
              (into [command new-props]
                    (map (if update? identity (partial step new-commands)))
                    (if props? more-w-props more-no-props))))]
    (step [] cli)))

(defn flatten-cli
  [cli]
  (letfn [(step [acc commands [command & [?conf :as cli]]]
            (let [conf (when (map? ?conf) ?conf)
                  new-commands (conj commands command)
                  new-acc (assoc acc new-commands conf)]
              (->> (filter vector? cli)
                   (map #(step new-acc new-commands %))
                   (reduce merge new-acc))))]
    (step {} [] cli)))

(defn get-props [cli commands]
  (get (flatten-cli cli) commands))

(defn conj-some [coll & xs]
  (apply conj coll (remove nil? xs)))

(defn add-opt [opt-specs [short-opt long-opt & more]]
  (let [compiled-opts (cli*/compile-option-specs opt-specs)
        [short-opts long-opts] (map #(set (map % compiled-opts))
                                    [:short-opt :long-opt])
        opt-spec (condp = [(contains? short-opts short-opt)
                           (contains? long-opts long-opt)]
                   [true true] nil
                   [false true] nil
                   [true false] (into [nil long-opt] more)
                   [false false] (into [short-opt long-opt] more))]
    (conj-some opt-specs opt-spec)))

(defn normalize [cli]
  (map-props cli identity))

(comment

  (normalize ["foo" ["bar"]])

  )

(defn right-pad [s n ch]
  (apply str s (repeat (- n (count s)) ch)))

(comment
  (right-pad "foo" 6 \space)

  (right-pad "foo" 2 \space)

  )

(comment
  (add-opt
   [["-h" "--hello-world" "Hi" :type :foo]]
   ["-h" "--help" "help"])

  (add-opt
   [[nil "--hello-world" "Hi" :type :foo]]
   ["-h" "--help" "help"])

  (add-opt
   [[nil "--help" "My-help" :type :foo]]
   ["-h" "--help" "help"])

  )

(comment

  (get-props
   ["foo"
    ["bar" {:x 1}]]
   ["foo" "bar"])

  (update-props
   ["foo" {:x 1}
    ["bar" {:x 1}]]
   ["foo" "bar"]
   assoc :y 12)

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

(defn cli->subcommands-map [cli]
  (letfn [(step [acc [cmd & more]]
            (assoc acc cmd (reduce step {} (filter vector? more))))]
    (reduce step {} (filter vector? cli))))

(defn cli->commands-map [cli]
  {(first cli) (cli->subcommands-map cli)})

(comment

  (cli->subcommands-map
   ["foo" {}
    ["bar" ["baz" {}]]
    ["quux" ["asd"]]])

  (cli->commands-map
   ["foo" {}
    ["bar" ["baz" {}]]
    ["quux" ["asd"]]])

  (cli->subcommands-map ["foo"])

  )

(defn update-existing [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn map-kvs [f m]
  (into {} (map f) m))

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

(defn text [lines-or-str]
  (cond-> lines-or-str
    (coll? lines-or-str)
    (->> (remove nil?) (str/join "\n"))))
