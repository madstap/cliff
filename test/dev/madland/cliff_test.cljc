(ns dev.madland.cliff-test
  (:require [dev.madland.cliff :as cliff]
            [dev.madland.cliff.vendor.tools-cli :as cli*]
            #?(:bb [dev.madland.matcher-combinators :refer [match?]]
               :clj [matcher-combinators.test :refer [match?]])
            [clojure.test :refer [deftest testing is]]))

(deftest sopt-lopt-desc-map
  (is (= {:short-opt "-f",
          :long-opt "--force",
          :desc "Force thing",
          :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map ["-f" "--force" "Force thing" :foo 1])))
  (is (= {:short-opt nil,
          :long-opt "--force",
          :desc "Force thing",
          :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map ["--force" "Force thing" :foo 1])))
  (is (= {:short-opt nil,
          :long-opt nil,
          :desc "Force thing",
          :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map ["Force thing" :foo 1])))
  (is (= {:short-opt nil, :long-opt "--force", :desc nil, :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map ["--force" :foo 1])))
  (is (= {:short-opt "-f", :long-opt nil, :desc nil, :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map ["-f" :foo 1])))
  (is (= {:short-opt "-f", :long-opt nil, :desc nil, :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map ["-f" nil :foo 1])))
  (is (= {:short-opt nil, :long-opt "--force", :desc nil, :spec-map {:foo 1}}
         (cli*/sopt-lopt-desc-map [nil "--force" :foo 1]))))

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

(defn reset-fn [a]
  (fn [ctx]
    (reset! a ctx)))

(defn throw-fn [ctx]
  (throw (ex-info "" ctx)))

(def ambiguous1
  ["amb" {:opts [[nil "--aa"] [nil "--bb"]]
          :args [{:id :x}]
          :handler identity}
   ["nested" {:handler identity}]])

(def ambiguous2
  ["amb" {:opts [[nil "--aa"] [nil "--bb"]]
          :args [{:id :xs
                  :varargs true}]
          :handler identity}
   ["nested" {:handler identity}]])

(deftest ambiguous
  (is (match? {::cliff/commands ["amb" "nested"],
               ::cliff/errors nil,
               ::cliff/handler fn?}
              (cliff/parse-args ["nested"] ambiguous1)))

  (is (match? {::cliff/commands ["amb" "nested"],
               ::cliff/errors nil,
               ::cliff/handler fn?}
              (cliff/parse-args ["nested"] ambiguous2))))

(deftest order-independent-opts-and-args
  (is (match? {:x "foo"
               :aa true
               :bb true}
              (cliff/parse-args ["--aa" "foo" "--bb"]
                                ["foo" {:opts [[nil "--aa"] [nil "--bb"]]
                                        :args [{:id :x}]
                                        :handler identity}])))

  (testing "ambiguous"
    (is (match? {::cliff/commands ["amb"],
                 ::cliff/parsed-options
                 {:aa {:value true ::cliff/commands ["amb"]}
                  :bb {:value true ::cliff/commands ["amb"]}},
                 ::cliff/errors nil,
                 ::cliff/arguments
                 {:x {::cliff/commands ["amb"], :value "foo"}},
                 ::cliff/handler fn?
                 :x "foo"
                 :aa true
                 :bb true}
                (cliff/parse-args ["--aa" "foo" "--bb"] ambiguous1)))))

(deftest parse-args-test
  (is (match? {::cliff/commands ["cmd"],
               ::cliff/parsed-options
               {:foo {:initial-value true
                      :value true
                      ::cliff/commands ["cmd"]}},
               ::cliff/errors nil,
               ::cliff/handler fn?
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
               ::cliff/handler fn?
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
               ::cliff/handler fn?
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
               ::cliff/handler fn?
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
               ::cliff/handler fn?
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
               ::cliff/handler fn?
               :git-dir "/other/proj/.git"
               :worktree "foo"
               :new-path "bar"}
              (cliff/parse-args ["--git-dir=/other/proj/.git" "worktree" "move" "foo" "bar"] git)))
  (is (match? {:foo true}
              (cliff/parse-args ["--foo"]
                                ["cmd" {:opts [["--foo" "foo"]]
                                        :handler identity}]))))

(defn a&w [line]
  (cliff/args-and-word line (count line)))

(deftest args-and-word-test
  (is (= [["bar"] "b"] (cliff/args-and-word "foo bar baz" 9)))
  (is (= [[] "bar"] (cliff/args-and-word "foo bar baz" 7)))
  (is (= [[] ""] (a&w "foo ")))
  (is (= [[] ""] (a&w "foo")))
  (is (= [[] ""] (a&w "")))
  (is (= [["--foo"] ""] (a&w "foo --foo=")))
  (is (= [["--foo"] "b"] (a&w "foo --foo=b"))))

(comment

  ;; TODO: Bad error, should validate and give better errors than this.
  (cli/parse-opts [] [[nil "foo" "Some docs"]])
  ;; Unhandled java.lang.AssertionError
  ;; Assert failed: (every? :id %)

  )
