(ns test-runner
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.test :as test]))

;; Adapted from:
;; https://gist.github.com/thiagokokada/fee513a7b8578c87c05469267c48e612

(defn test-file->test-ns
  [file]
  (as-> file $
    (fs/components $)
    (drop 1 $)
    (mapv str $)
    (str/join "." $)
    (str/replace $ #"_" "-")
    (str/replace $ #"\.cljc?$" "")
    (symbol $)))

(def dir "test")

(def exts #{"clj" "cljc"})

(defn run []
  (let [test-namespaces (->> exts
                             (mapcat #(fs/glob dir (str "**/*_test." %)))
                             (mapv test-file->test-ns))
        _ (apply require test-namespaces)
        test-results (apply test/run-tests test-namespaces)
        {:keys [fail error]} test-results
        exit (+ fail error)]
    ;; Babashka considers a task not successfull if it throws an
    ;; exception, even with {:babashka/exit 0}
    (when-not (zero? exit)
      (throw (ex-info "Tests failed" {:babashka/exit exit})))))
