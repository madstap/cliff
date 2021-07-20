(ns dev.madland.cliff.types
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [dev.madland.cliff.utils :as utils]))

(defn not-file? [file]
  (or (not (fs/exists? file)) (fs/directory? file)))

(defn not-dir? [file]
  (or (not (fs/exists? file)) (not (fs/directory? file))))

(defn parse-keyword [s]
  (keyword (second (re-find #"^:?(.*)$" s))))

(defn bare-keyword-str [k]
  (str (some-> (namespace k) (str "/")) (name k)))

(defn has-whitespace? [s]
  (not (re-find #"\s" s)))

(defn expand-tilde [s]
  (str/replace s #"^~" (System/getenv "HOME")))

(defn ls [dir]
  (map #(if (fs/directory? %)
          {:candidate (str % "/")
           :on-complete :continue}
          {:candidate (str %)
           :on-complete :next})
       (fs/list-dir (fs/file (expand-tilde dir)))))

:complete/sources
:complete/filters

;; A source is a function that produces a sequence of suggestions.
;; Is currently passed the opt or arg map, which is useful for passing
;; :fs/dir for example.

;; A filter is a predicate that filters each suggestion from a source.
;; Currently arity [opt-map word].

(defn enum-source [{:keys [values dev.madland/word]}]
  (map (if (and (some? word) (str/starts-with? word ":"))
         str
         bare-keyword-str)
       values))

;; TODO: Support windows
(defn source-dir [dir word]
  (let [non-blank-dir? (not (str/blank? dir))
        file-sep-in-word? (str/includes? word "/")]
    (str (when non-blank-dir? dir)
         (when (and non-blank-dir? file-sep-in-word?) "/")
         (when file-sep-in-word?
           (str (cond->> (str/split word #"/")
                  (not (str/ends-with? word "/")) butlast
                  true (str/join "/"))
                "/")))))

(comment

  (= "" (source-dir "" "foo"))

  (= "foo/" (source-dir "" "foo/"))

  (= "foo/" (source-dir "" "foo/b"))

  )

(def types
  {:int {:parse [#(Long/parseLong %) "Invalid format for int"]}
   :float {:parse [#(Double/parseDouble %) "Invalid format for float"]}
   :port {:parse [:int]
          :validate
          [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]}
   :string {}
   :url {} ;; TODO: validate
   :host {} ;; java.lang.InetHost ??? :net-host :Inet-host
   :url/host {} ;; TODO: validate
   :url/path {} ;; TODO: validate
   :file {:parse [fs/file ""]
          :validate [not-dir? "Can't be a directory"]
          :complete/sources [(fn [{:keys [fs/dir dev.madland/word]
                                   :or {dir ""}}]
                               (ls (source-dir dir word)))]}
   :dir {:parse [fs/file ""]
         :validate [not-file? "Can't be a file"]
         :complete/sources [:file]
         :complete/filters [#(fs/directory? %2)]}

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
    ;; validation functions could receive the props as well as the value.
    :validate [(constantly true) "???"]
    :complete/sources [enum-source]}})

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

(defn invoke-completions [types {:keys [type] :as opt} word]
  (let [{:complete/keys [sources filters]} (types type)
        srcs (resolve-pointers types :complete/sources sources)
        preds (resolve-pointers types :complete/filters filters)
        opt (assoc opt :dev.madland/word word)]
    (->> srcs
         (mapcat #(% opt))
         (filter (fn [completion]
                   (let [{:keys [on-complete candidate]}
                         (if (map? completion)
                           completion
                           {:candidate completion})]
                     (every? #(% (utils/assoc-some opt :on-complete on-complete)
                                 candidate)
                             preds)))))))

(comment

  (invoke-completions types {:type :file} "")


  )

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
