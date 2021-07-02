(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [babashka.fs :as fs]))

(defn read-arguments [arguments arg-config]
  ;; TODO: Error handling
  (let [[normal-args [{vararg-id :id}]]
        (if (:varargs (first arg-config))
          [[] arg-config]
          (partition-by :varargs arg-config))

        normal-parsed (->> (map (fn [arg {:keys [id] :as cfg}]
                                  [id arg])
                                arguments
                                normal-args)
                           (into {}))
        vararg-vals (vec (drop (count normal-args) arguments))]
    (cond-> normal-parsed
      (some? vararg-id) (assoc vararg-id vararg-vals))))

(defn flatten-command-decl
  [command-decl]
  (letfn [(step [acc commands [command & [?conf :as command-decl]]]
            (let [conf (when (map? ?conf) ?conf)
                  new-commands (conj commands command)
                  new-acc (assoc acc new-commands conf)]
              (->> (filter vector? command-decl)
                   (map #(step new-acc new-commands %))
                   (reduce merge new-acc))))]
    (step {} [] command-decl)))

(defn next-commands [commands commands->opts]
  (->> (keys commands->opts)
       (filter #(= commands (butlast %)))
       (map last)
       set
       not-empty))

(defn conj-some [coll & xs]
  (apply conj coll (remove nil? xs)))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

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

;; Do the initial parsing, collecting all the strings
(defn parse-args-1
  [arguments [app-name :as command-decl]]
  (let [commands->opts (flatten-command-decl command-decl)]
    (loop [{::keys [commands], :as ctx} {::commands [app-name]
                                         ::parsed-options {}
                                         ::errors nil}
           arguments arguments]
      (let [{:keys [opts handler args] :as props}
            (commands->opts commands)

            ;; TODO: error handling
            {:keys [options errors] new-arguments :arguments :as parsed}
            (if opts
              (cli/parse-opts arguments (remove-unknown-keys opts)
                              :in-order (nil? args))
              {:arguments arguments})

            new-ctx
            (-> ctx
                (update ::parsed-options merge
                        (map-vals (fn [v] {:value v
                                           ::commands commands})
                                  options)))]

        (cond (and (nil? handler)
                   (empty? new-arguments))
              (update new-ctx ::errors conj "Insufficient input")

              (some? args)
              ;; TODO: Error handling here.
              (let [parsed-args
                    (->> (read-arguments new-arguments args)
                         (map-vals #(hash-map :value % ::commands commands)))]
                (-> new-ctx
                    (assoc ::arguments parsed-args ::handler handler)))

              :else
              (if-some [nxt (next-commands commands commands->opts)]
                (let [[command & more-args] new-arguments]
                  (if (contains? nxt command)
                    (recur (update new-ctx ::commands conj command)
                           more-args)
                    (update new-ctx ::errors conj
                            (str "Unknown command " command))))
                ;; TODO: This means that handlers can only be leaves.
                ;;       which is probably not necessary.
                (if (some? handler)
                  (assoc new-ctx ::handler handler)
                  (update new-ctx ::errors (fnil conj [])
                          (str "No handler for " commands)))))))))

(defn parsed-values [parsed-options]
  (map-vals :value parsed-options))


;; Get a map of the keys and values we're actually after, merge that into the
;; top level of the context.
(defn merge-config-to-top-level
  [{::keys [commands parsed-options arguments] :as parsed}]
  (let [config (merge (parsed-values parsed-options)
                      (parsed-values arguments))]
    (merge parsed config)))

(defn parse-args [arguments command-decl]
  (-> (parse-args-1 arguments command-decl)
      merge-config-to-top-level))

(defn run! [args command-decl]
  (let [{::keys [handler] :as ctx} (parse-args args command-decl)]
    (handler ctx)))

(defn bb! [command-decl]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* command-decl)))
