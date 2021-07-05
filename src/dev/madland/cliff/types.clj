(ns dev.madland.cliff.types
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

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
   :string {}
   :url {} ;; TODO: validate
   :url/host {} ;; TODO: validate
   :url/path {} ;; TODO: validate
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
    [has-whitespace? "Can't have whitespace in symbols."]}

   :enum
   {:parse [:keyword]
    :pre-validate [:keyword]
    ;; TODO: How to do this?
    :validate [(constantly true) "???"]}})

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
      (-> (let [{v :value parse-errors :errors} (invoke-parse types parse value)]
            (if parse-errors
              {:errors parse-errors
               :value v}
              (if-some [errors (invoke-validate types :validate validate v)]
                {:errors errors
                 :value v}
                {:value v})))
          (assoc :initial-value value)))))
