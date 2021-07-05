(ns dev.madland.cliff-test
  (:require [dev.madland.cliff :as cliff]
            #?(:bb [dev.madland.matcher-combinators :refer [match?]]
               :clj [matcher-combinators.test :refer [match?]])
            [clojure.test :refer [deftest testing is]]))

(deftest read-arguments-test
  (is (= {:x 1, :y 2} (cliff/read-arguments [1 2] [{:id :x} {:id :y}])))
  (is (= {:xs [1 2 3]}
         (cliff/read-arguments [1 2 3] [{:id :xs :varargs true}])))
  (is (= {:x 1 :xs [2 3]}
         (cliff/read-arguments [1 2 3] [{:id :x} {:id :xs :varargs true}]))))

(defn log [ctx])

(defn worktree-add [ctx])

(defn worktree-move [ctx])

(def git
  ["git" {:opts [["-d" "--git-dir DIR" "git dir"]]
          :doc "Git, a simple, easy to use ... haha, jk"
          :version "1.2.3"}
   ["log" {:opts [["-o" "--oneline" "asd"]
                  ["-g" "--graph" "dsa"]]
           :handler log}]
   ["worktree"
    ["add" {:opts [["-b" nil
                    :id :branch
                    :required "BRANCH"]]
            :handler worktree-add}]
    ["move" {:args [{:id :worktree}
                    {:id :new-path}]
             :handler worktree-move}]]])

(deftest parse-args-types-test
  (is (match? {:foo 123}
              (cliff/parse-args ["--foo" "123"]
                                ["cmd" {:opts [["-f" "--foo X" "foo"
                                                :type :int]]
                                        :handler identity}])))
  (is (match? {:foo 123}
              (cliff/parse-args ["123"]
                                ["cmd" {:args [{:id :foo :type :int}]
                                        :handler identity}]))))

(defn =fn [f] #(= % f))

(def nested #(identity %))

(def ambiguous1
  ["amb" {:opts [[nil "--aa"] [nil "--bb"]]
          :args [{:id :x}]
          :handler identity}
   ["nested" {:handler nested}]])

(def ambiguous2
  ["amb" {:opts [[nil "--aa"] [nil "--bb"]]
          :args [{:id :xs
                  :varargs true}]
          :handler identity}
   ["nested" {:handler nested}]])

#_
(deftest ambiguous
  (is (match? {::cliff/commands ["amb" "nested"],
               ::cliff/parsed-options {},
               ::cliff/errors nil,
               ::cliff/arguments {},
               ::cliff/handler (=fn nested)}
              (cliff/parse-args ["nested"] ambiguous1)))

  (is (match? {::cliff/commands ["amb" "nested"],
               ::cliff/parsed-options {},
               ::cliff/errors nil,
               ::cliff/arguments {},
               ::cliff/handler (=fn nested)}
              (cliff/parse-args ["nested"] ambiguous2))))

(deftest order-independent-opts-and-args
  (is (match? {:x "foo"
               :aa true
               :bb true}
              (cliff/parse-args ["--aa" "foo" "--bb"]
                                ["foo" {:opts [[nil "--aa"] [nil "--bb"]]
                                        :args [{:id :x}]
                                        :handler identity}])))
  #_(testing "ambiguous"
    (is (match? {::cliff/commands ["amb"],
                 ::cliff/parsed-options
                 {:aa {:value true ::cliff/commands ["amb"]}
                  :bb {:value true ::cliff/commands ["amb"]}},
                 ::cliff/errors nil,
                 ::cliff/arguments
                 {:x {::cliff/commands ["amb"], :value "foo"}},
                 ::cliff/handler (=fn identity)
                 :x "foo"
                 :aa true
                 :bb true}
                (cliff/parse-args ["-a" "foo" "-b"] ambiguous1)))))

(deftest parse-args-test
  (is (match? {::cliff/commands ["cmd"],
               ::cliff/parsed-options
               {:foo {:initial-value true
                      :value true
                      ::cliff/commands ["cmd"]}},
               ::cliff/errors nil,
               ::cliff/handler (=fn identity)
               :foo true}
              (cliff/parse-args ["--foo"]
                                ["cmd" {:opts [["-f" "--foo" "foo"]]
                                        :handler identity}])))
  (is (match? {::cliff/commands ["cmd"],
               ::cliff/parsed-options
               {:foo {:initial-value true
                      :value true
                      ::cliff/commands ["cmd"]}},
               ::cliff/errors nil,
               ::cliff/arguments
               {:bar {:initial-value "foobar"
                      :value "foobar"
                      ::cliff/commands ["cmd"]}},
               ::cliff/handler (=fn identity)
               :foo true,
               :bar "foobar"}
              (cliff/parse-args ["--foo" "foobar"]
                                ["cmd" {:opts [["-f" "--foo" "foo"]]
                                        :args [{:id :bar}]
                                        :handler identity}])))
  (is (match? {::cliff/commands ["cmd" "nested"]
               ::cliff/parsed-options
               {:foo {:initial-value true
                      :value true
                      ::cliff/commands ["cmd"]}}
               ::cliff/errors nil
               ::cliff/arguments
               {:bar {:initial-value "foobar"
                      :value "foobar"
                      ::cliff/commands ["cmd" "nested"]}}
               ::cliff/handler (=fn identity)
               :foo true
               :bar "foobar"}
              (cliff/parse-args ["--foo" "nested" "foobar"]
                                ["cmd" {:opts [["-f" "--foo" "foo"]]}
                                 ["nested" {:handler identity
                                            :args [{:id :bar}]}]]))))

(deftest git-parse-args-test
  (is (match? {::cliff/commands ["git" "log"]
               ::cliff/parsed-options
               {:git-dir {:initial-value "/other/proj/.git"
                          :value "/other/proj/.git"
                          ::cliff/commands ["git"]}
                :oneline {:initial-value true
                          :value true
                          ::cliff/commands ["git" "log"]}
                :graph {:initial-value true
                        :value true
                        ::cliff/commands ["git" "log"]}}
               ::cliff/errors nil
               ::cliff/handler (=fn log)
               :git-dir "/other/proj/.git"
               :oneline true
               :graph true}
              (cliff/parse-args ["--git-dir=/other/proj/.git" "log" "--oneline" "--graph"] git)))
  (is (match? {::cliff/commands ["git" "worktree" "add"]
               ::cliff/parsed-options
               {:git-dir {:initial-value "/other/proj/.git"
                          :value "/other/proj/.git"
                          ::cliff/commands ["git"]}
                :branch {:initial-value "foo"
                         :value "foo"
                         ::cliff/commands ["git" "worktree" "add"]}}
               ::cliff/errors nil
               ::cliff/handler (=fn worktree-add)
               :git-dir "/other/proj/.git"
               :branch "foo"}
              (cliff/parse-args ["--git-dir=/other/proj/.git" "worktree" "add" "-b" "foo"] git)))
  (is (match? {::cliff/commands ["git" "worktree" "move"]
               ::cliff/parsed-options
               {:git-dir {:initial-value "/other/proj/.git"
                          :value "/other/proj/.git"
                          ::cliff/commands ["git"]}}
               ::cliff/errors nil
               ::cliff/arguments
               {:worktree  {:initial-value "foo"
                            :value "foo"
                            ::cliff/commands ["git" "worktree" "move"]}
                :new-path {:initial-value "bar"
                           :value "bar"
                           ::cliff/commands ["git" "worktree" "move"]}}
               ::cliff/handler (=fn worktree-move)
               :git-dir "/other/proj/.git"
               :worktree "foo"
               :new-path "bar"}
              (cliff/parse-args ["--git-dir=/other/proj/.git" "worktree" "move" "foo" "bar"] git))))

(comment

  (def calc
    ["toycalc" {:opts [[nil "--base BASE" "The number base for output"
                        :parse-fn #(Long/parseLong %)
                        :default 10]]
                :version "0.0.1"}
     ["add" {:opts []}]
     ["subtract" {:opts []}]])

  )

(comment

  ;; TODO: Bad error, should validate and give better errors than this.
  (cli/parse-opts [] [[nil "foo" "Some docs"]])
  ;; Unhandled java.lang.AssertionError
  ;; Assert failed: (every? :id %)

  )
