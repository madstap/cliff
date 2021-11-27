(ns dev.madland.cliff.vendor.tools-cli
  (:require [clojure.string :as str]))

(defn- split-opt [pred [x & xs :as coll]]
  (if (or (nil? x) (and (string? x) (pred x)))
    [x xs]
    [nil coll]))

(defn sopt-lopt-desc-map [spec]
  (let [[sopt more] (split-opt #(re-find #"^-[^-]" %) spec)
        [lopt more] (split-opt #(str/starts-with? % "--") more)
        [desc more] (split-opt string? more)]
    {:short-opt sopt
     :long-opt lopt
     :desc desc
     :spec-map (apply hash-map more)}))

(defn- compile-spec [spec]
  (let [{:keys [short-opt long-opt desc spec-map]} (sopt-lopt-desc-map spec)
        long-opt (or long-opt (:long-opt spec-map))
        [long-opt req] (when long-opt
                         (rest (re-find #"^(--[^ =]+)(?:[ =](.*))?" long-opt)))
        id (when long-opt
             (keyword (nth (re-find #"^--(\[no-\])?(.*)" long-opt) 2)))
        validate (:validate spec-map)
        [validate-fn validate-msg] (when (seq validate)
                                     (->> (partition 2 2 (repeat nil) validate)
                                          (apply map vector)))]
    (merge {:id id
            :short-opt short-opt
            :long-opt long-opt
            :required req
            :desc desc
            :validate-fn validate-fn
            :validate-msg validate-msg}
           (dissoc spec-map :validate))))

(defn- distinct?* [coll]
  (if (seq coll)
    (apply distinct? coll)
    true))

(defn- wrap-val [map key]
  (if (contains? map key)
    (update-in map [key] #(cond (nil? %) nil
                                (coll? %) %
                                :else [%]))
    map))

(defn compile-option-specs
  [option-specs]
  {:post [(every? :id %)
          (distinct?* (map :id (filter :default %)))
          (distinct?* (map :id (filter :default-fn %)))
          (distinct?* (remove nil? (map :short-opt %)))
          (distinct?* (remove nil? (map :long-opt %)))
          (every? (comp not (partial every? identity))
                  (map (juxt :assoc-fn :update-fn) %))]}
  (map (fn [spec]
         (-> (if (map? spec)
               spec
               (compile-spec spec))
             (wrap-val :validate-fn)
             (wrap-val :validate-msg)))
       option-specs))

(def tools-cli-opt-keys
  [:id :short-opt :long-opt :required :desc :default :default-desc :default-fn
   :parse-fn :assoc-fn :update-fn :multi :post-validation
   :validate-fn :validate-msg :missing])

(defn remove-unknown-keys [opts]
  (mapv #(if (map? %)
           (select-keys % tools-cli-opt-keys)
           (let [[sopt-lopt-desc kvs] (split-with (some-fn string? nil?) %)
                 m (apply hash-map kvs)]
             (into (vec sopt-lopt-desc)
                   cat
                   (select-keys m tools-cli-opt-keys))))
        opts))

(defn tokenize-args
  "Reduce arguments sequence into [opt-type opt ?optarg?] vectors and a vector
  of remaining arguments. Returns as [option-tokens remaining-args].

  Expands clumped short options like \"-abc\" into:
  [[:short-opt \"-a\"] [:short-opt \"-b\"] [:short-opt \"-c\"]]

  If \"-b\" were in the set of options that require arguments, \"-abc\" would
  then be interpreted as: [[:short-opt \"-a\"] [:short-opt \"-b\" \"c\"]]

  Long options with `=` are always parsed as option + optarg, even if nothing
  follows the `=` sign.

  If the :in-order flag is true, the first non-option, non-optarg argument
  stops options processing. This is useful for handling subcommand options."
  [required-set args & options]
  (let [{:keys [in-order]} (apply hash-map options)]
    (loop [opts [] argv [] [car & cdr] args]
      (if car
        (condp re-seq car
          ;; Double dash always ends options processing
          #"^--$" (recur opts (into argv cdr) [])
          ;; Long options with assignment always passes optarg, required or not
          #"^--\S+=" (recur (conj opts (into [:long-opt] (str/split car #"=" 2)))
                            argv cdr)
          ;; Long options, consumes cdr head if needed
          #"^--" (let [required? (contains? required-set car)
                       [optarg cdr] (if required?
                                      [(first cdr) (rest cdr)]
                                      [nil cdr])]
                   (recur (conj opts (into [:long-opt car] (if required? [optarg] [])))
                          argv cdr))
          ;; Short options, expands clumped opts until an optarg is required
          #"^-." (let [[os cdr] (loop [os [] [c & cs] (rest car)]
                                  (let [o (str \- c)]
                                    (if (contains? required-set o)
                                      (if (seq cs)
                                        ;; Get optarg from rest of car
                                        [(conj os [:short-opt o (str/join cs)]) cdr]
                                        ;; Get optarg from head of cdr
                                        [(conj os [:short-opt o (first cdr)]) (rest cdr)])
                                      (if (seq cs)
                                        (recur (conj os [:short-opt o]) cs)
                                        [(conj os [:short-opt o]) cdr]))))]
                   (recur (into opts os) argv cdr))
          (if in-order
            (recur opts (into argv (cons car cdr)) [])
            (recur opts (conj argv car) cdr)))
        [opts argv]))))
