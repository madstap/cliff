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
    (str/replace $ #".clj$" "")
    (symbol $)))

(defn run []
  (let [test-namespaces (->> (fs/glob "test" "**/*_test.clj")
                             (mapv test-file->test-ns))
        _ (apply require test-namespaces)
        test-results (apply test/run-tests test-namespaces)
        {:keys [fail error]} test-results]
    (System/exit (+ fail error))))
