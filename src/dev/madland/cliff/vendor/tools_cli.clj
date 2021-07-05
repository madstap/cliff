(ns dev.madland.cliff.vendor.tools-cli
  "")

(defn- compile-spec [spec]
  (let [sopt-lopt-desc (take-while #(or (string? %) (nil? %)) spec)
        spec-map (apply hash-map (drop (count sopt-lopt-desc) spec))
        [short-opt long-opt desc] sopt-lopt-desc
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
