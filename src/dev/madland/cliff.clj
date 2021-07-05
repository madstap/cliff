(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [dev.madland.cliff.utils :as utils]
            [dev.madland.cliff.types :as types]
            [dev.madland.cliff.vendor.tools-cli :as cli*]
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

            id->compiled-opts (->> (cli*/compile-option-specs opts)
                                   (utils/index-by :id))

            id->arg-config (utils/index-by :id args)

            ;; TODO: error handling
            {:keys [options errors] new-arguments :arguments :as parsed}
            (if opts
              (cli/parse-opts arguments (cli*/remove-unknown-keys opts)
                              :in-order (nil? args))
              {:arguments arguments})

            new-ctx
            (-> ctx
                (update ::parsed-options merge
                        (utils/map-kv-vals (fn [k v]
                                             (merge (id->compiled-opts k)
                                                    {:id k
                                                     :value v
                                                     ::commands commands}))
                                           options)))]

        (cond (and (nil? handler)
                   (empty? new-arguments))
              (update new-ctx ::errors conj "Insufficient input")

              :else
              (let [nxt (next-commands commands commands->opts)]
                (let [[command & more-args] new-arguments]
                  (if (contains? nxt command)
                    (recur (update new-ctx ::commands conj command)
                           more-args)
                    ;; TODO: Error handling here.
                    (let [parsed-args
                          (if (nil? args)
                            nil
                            (->> (read-arguments new-arguments args)
                                 (utils/map-kv-vals
                                  (fn [k v]
                                    (merge (id->arg-config k)
                                           {:id k
                                            :value v
                                            ::commands commands})))))

                          new-new-ctx (utils/assoc-some new-ctx ::arguments parsed-args)
                          new-new-arguments (drop (count parsed-args)
                                                  new-arguments)]

                      (-> new-new-ctx
                          (utils/assoc-some ::handler handler)
                          (cond-> (nil? handler)
                            (update ::errors (fnil conj [])
                                    (str "No handler for " commands))

                            (seq new-new-arguments)
                            (-> (assoc ::extra-input new-new-arguments)
                                (update ::errors (fnil conj [])
                                        "Extra input")))))))))))))

(defn parsed-values [parsed-options]
  (utils/map-vals :value parsed-options))

(defn collect-errors [{::keys [errors parsed-options arguments]}]
  ;; TODO:
  )

(defn apply-defaults [parsed command-decl]
  parsed)

(defn parse-and-validate [parsed-values commands->opts]
  (utils/map-vals
   (fn [{:keys [type value]
         :or {type :string}
         :as parsed-val}]
     (-> (merge parsed-val
                (types/parse-and-validate types/types type value))
         (assoc :initial-value value)))
   parsed-values))

(defn parse-and-validate-all [parsed command-decl]
  (let [commands->opts (flatten-command-decl command-decl)]
    (-> parsed
        (utils/update-existing ::parsed-options
                               parse-and-validate commands->opts)
        (utils/update-existing ::arguments
                               parse-and-validate commands->opts))))

;; Get a map of the keys and values we're actually after, merge that into the
;; top level of the context.
(defn merge-config-to-top-level
  [{::keys [commands parsed-options arguments] :as parsed}]
  (let [config (merge (parsed-values parsed-options)
                      (parsed-values arguments))]
    (merge parsed config)))

(defn parse-args [arguments command-decl]
  (-> (parse-args-1 arguments command-decl)
      (apply-defaults command-decl)
      (parse-and-validate-all command-decl)
      merge-config-to-top-level))

(defn run! [args [_ global-props :as command-decl]]
  (let [{::keys [handler] :as ctx} (parse-args args command-decl)]
    (handler ctx)))

(defn bb! [command-decl]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* command-decl)))
