(ns dev.madland.completions-test
  (:require [clojure.test :refer [deftest testing is]]
            [dev.madland.cliff :as cliff]
            [clojure.string :as str]))

(defn cmps [line cli]
  (cliff/completions line (count line) cli))

(deftest completions-test
  (testing (str/join "\n"
                     ["When there are possible commands and possible options, "
                      "just suggest the commands and not the options."])
    (is (= [{:candidate "foo"
             :on-complete :next}
            {:candidate "bar"
             :on-complete :next}]
           (cmps "prog "
                 ["prog" {:opts [[nil "--foo"]]}
                  ["foo" {:handler identity}]
                  ["bar" {:handler identity}]]))))

  (testing "When word is --, suggest options"
    (testing "without arguments"
      (is (= [{:candidate "--foo"
               :on-complete :next}]
             (cmps "prog --"
                   ["prog" {:opts [[nil "--foo"]]}
                    ["foo" {:handler identity}]
                    ["bar" {:handler identity}]]))))
    (testing "with arguments"
      (is (= [{:candidate "--foo="
               :on-complete :continue}]
             (cmps "prog --"
                   ["prog" {:opts [[nil "--foo FOO"]]}
                    ["foo" {:handler identity}]
                    ["bar" {:handler identity}]])))))

  (testing "when word is a long opt with an arg"

    (testing "with trailing ="
      (is (= [{:candidate "foo"
               :on-complete :next}
              {:candidate "bar"
               :on-complete :next}]
             (cmps "prog --foo="
                   ["prog" {:opts [[nil "--foo FOO"
                                    :type :enum
                                    :values [:foo :bar]]]}])))
      (is (= [{:candidate "foo"
               :on-complete :next}]
             (cmps "prog --foo=f"
                   ["prog" {:opts [[nil "--foo FOO"
                                    :type :enum
                                    :values [:foo :bar]]]}]))))

    (testing "next word"
      (is (= [{:candidate "foo"
               :on-complete :next}
              {:candidate "bar"
               :on-complete :next}]
             (cmps "prog --foo "
                   ["prog" {:opts [[nil "--foo FOO"
                                    :type :enum
                                    :values [:foo :bar]]]}])))
      (is (= [{:candidate "foo"
               :on-complete :next}]
             (cmps "prog --foo f"
                   ["prog" {:opts [[nil "--foo FOO"
                                    :type :enum
                                    :values [:foo :bar]]]}]))))))
