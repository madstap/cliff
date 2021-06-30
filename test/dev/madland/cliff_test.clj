(ns dev.madland.cliff-test
  (:require [dev.madland.cliff :as cliff]
            [clojure.test :refer [deftest is]]))

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

(deftest parse-args-test
  (is (= {::cliff/commands ["cmd"],
          ::cliff/parsed-options
          [{:foo true, ::cliff/commands ["cmd"]}],
          ::cliff/errors nil,
          ::cliff/handler identity
          :foo true}
         (cliff/parse-args ["--foo"]
                           ["cmd" {:opts [["-f" "--foo" "foo"]]
                                   :handler identity}])))
  (is (= {::cliff/commands ["cmd"],
          ::cliff/parsed-options
          [{:foo true, ::cliff/commands ["cmd"]}],
          ::cliff/errors nil,
          ::cliff/arguments
          {:bar "foobar", ::cliff/commands ["cmd"]},
          ::cliff/handler identity
          :foo true,
          :bar "foobar"}
         (cliff/parse-args ["--foo" "foobar"]
                           ["cmd" {:opts [["-f" "--foo" "foo"]]
                                   :args [{:id :bar}]
                                   :handler identity}])))
  (is (= {::cliff/commands ["cmd" "nested"]
          ::cliff/parsed-options
          [{:foo true, ::cliff/commands ["cmd"]}]
          ::cliff/errors nil
          ::cliff/arguments
          {:bar "foobar", ::cliff/commands ["cmd" "nested"]}
          ::cliff/handler identity
          :foo true
          :bar "foobar"}
         (cliff/parse-args ["--foo" "nested" "foobar"]
                           ["cmd" {:opts [["-f" "--foo" "foo"]]}
                            ["nested" {:handler identity
                                       :args [{:id :bar}]}]]))))

(deftest git-parse-args-test
  (is (= {::cliff/commands ["git" "log"]
          ::cliff/parsed-options
          [{:git-dir "/other/proj/.git" ::cliff/commands ["git"]}
           {:oneline true
            :graph true
            ::cliff/commands ["git" "log"]}]
          ::cliff/errors nil
          ::cliff/handler log
          :git-dir "/other/proj/.git"
          :oneline true
          :graph true}
         (cliff/parse-args ["--git-dir=/other/proj/.git" "log" "--oneline" "--graph"] git)))
  (is (= {::cliff/commands ["git" "worktree" "add"]
          ::cliff/parsed-options
          [{:git-dir "/other/proj/.git" ::cliff/commands ["git"]}
           {:branch "foo"
            ::cliff/commands ["git" "worktree" "add"]}]
          ::cliff/errors nil
          ::cliff/handler worktree-add
          :git-dir "/other/proj/.git"
          :branch "foo"}
         (cliff/parse-args ["--git-dir=/other/proj/.git" "worktree" "add" "-b" "foo"] git)))
  (is (= {::cliff/commands ["git" "worktree" "move"]
          ::cliff/parsed-options
          [{:git-dir "/other/proj/.git" ::cliff/commands ["git"]}]
          ::cliff/errors nil
          ::cliff/arguments
          {:worktree "foo"
           :new-path "bar"
           ::cliff/commands ["git" "worktree" "move"]}
          ::cliff/handler worktree-move
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
