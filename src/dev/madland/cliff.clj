(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [babashka.fs :as fs]))

(defn read-arguments [arguments arg-config]
  (->> (map (fn [arg {:keys [id] :as cfg}]
              [id arg])
            arguments
            arg-config)
       (into {})))

(defn not-file? [file]
  (or (not (fs/exists? file)) (fs/directory? file)))

(defn not-dir? [file]
  (or (not (fs/exists? file)) (not (fs/directory? file))))

(defn parse-keyword [s]
  (keyword (second (re-find #"^:?(.*)$" s))))

(defn has-whitespace? [s]
  (not (re-find #"\s" s)))

(def types
  {:int {:parse [#(Long/parseLong %) "Invalid format for int"]}
   :float {:parse [#(Double/parseDouble %) "Invalid format for float"]}
   :port {:parse [:int]
          :validate
          [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]}
   :url {} ;; TODO: validate
   :url/host {}
   :url/path {}
   :file {:parse [fs/file]
          :validate [not-dir? "Can't be a directory"]}
   :dir {:parse [fs/file]
         :validate [not-file? "Can't be a file"]}

   ;; TODO: Tagged literals/data readers
   :edn-file {:parse [:file slurp edn/read-string]
              :validate [:file]}
   :keyword
   {:parse [parse-keyword]
    :pre-validate
    [#(not (str/starts-with? % "::")) "Auto namespaced keywords not allowed"
     has-whitespace? "Can't have whitespace in keywords."]}
   :symbol
   {:parse [symbol]
    :pre-validate
    [has-whitespace? "Can't have whitespace in symbols."]}})

(defn partition-pairs [pairs]
  (partition-by symbol? pairs))

(defn resolve-pointers [types kind pairs]
  (if (some keyword? pairs)
    (->> pairs
         (mapcat (fn [x]
                   (if (keyword? x)
                     (get-in types [x kind])
                     [x])))
         (resolve-pointers types kind))
    pairs))

(defn invoke-parse [types parsers v]
  (reduce (fn [acc [f msg]]
            (let [[success? new-val-or-msg]
                  (try [true (f v)]
                       (catch Throwable e
                         [false (.getMessage e)]))]
              (if success?
                (assoc acc :value new-val-or-msg)
                (reduced (assoc acc :errors [(or msg new-val-or-msg)])))))
          {:value v
           :errors nil}
          (->> parsers (resolve-pointers types :parse) partition-pairs)))

(defn invoke-validate [types kind validators v]
  (reduce (fn [errors [pred msg]]
            (if (try (pred v) (catch Throwable _ false))
              errors
              (conj (or errors []) msg)))
          nil
          (->> validators (resolve-pointers types kind) partition-pairs)))

(defn parse-and-validate [types type value]
  (let [{:keys [pre-validate validate parse]} (types type)]
    (if-some [pre-errors (invoke-validate types :pre-validate pre-validate value)]
      {:errors pre-errors
       :value value}
      (let [{v :value parse-errors :errors} (invoke-parse types parse value)]
        (if parse-errors
          {:errors parse-errors
           :value v}
          (if-some [errors (invoke-validate types :validate validate v)]
            {:errors errors
             :value v}
            {:value v}))))))

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

(defn parse-args-1
  [arguments [app-name :as command-decl]]
  (let [commands->opts (flatten-command-decl command-decl)]
    (loop [{::keys [commands], :as ctx} {::commands [app-name]
                                         ::parsed-options []
                                         ::errors nil}
           arguments arguments]
      (let [{:keys [opts handler args] :as props}
            (commands->opts commands)

            {:keys [options errors] new-arguments :arguments :as parsed}
            (if opts
              (cli/parse-opts arguments opts :in-order (nil? args))
              {:arguments arguments})

            new-ctx
            (-> ctx
                (update ::parsed-options conj-some
                        (some-> options not-empty (assoc ::commands commands))))]

        (cond (and (nil? handler)
                   (empty? new-arguments))
              (update new-ctx ::errors conj "Insufficient input")

              (some? args)
              ;; TODO: Error handling here.
              (let [parsed-args (-> (read-arguments new-arguments args)
                                    (assoc ::commands commands))]
                (-> new-ctx
                    (assoc ::arguments parsed-args ::handler handler)))

              :else
              (if-some [nxt (next-commands commands commands->opts)]
                (let [[command & more-args] new-arguments]
                  (if (contains? nxt command)
                    (recur (update new-ctx ::commands conj command)
                           more-args)
                    (do (prn nxt new-arguments)
                        (update new-ctx ::errors conj
                                (str "Unknown command " command)))))
                (if (some? handler)
                  (assoc new-ctx ::handler handler)
                  (update new-ctx ::errors (fnil conj [])))))))))

(defn prep-parsed-opts [parsed-options]
  (transduce (map #(dissoc % ::commands)) merge parsed-options))

(defn prep-parsed-args [parsed-args]
  (dissoc parsed-args ::commands))

(defn parse-args-2 [{::keys [commands parsed-options arguments] :as parsed}]
  (let [config (merge (prep-parsed-opts parsed-options)
                      (prep-parsed-args arguments))]
    (merge parsed config)))

(defn parse-args [arguments command-decl]
  (-> (parse-args-1 arguments command-decl)
      (parse-args-2)))

(defn run! [args command-decl]
  (let [{::keys [handler] :as ctx} (parse-args args command-decl)]
    (handler ctx)))

(defn bb! [command-decl]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* command-decl)))
