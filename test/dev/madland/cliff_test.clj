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
    ["add" {:opts [["-b" "--b BRANCH"]]
            :handler worktree-add}]
    ["move" {:args [{:name :worktree}
                    {:name :new-path}]
             :handler worktree-move}]]])

(deftest parse-args-test
  (is (= {:commands ["git" "log"],
          :options
          {:git-dir "/other/proj/.git",
           :git/git-dir "/other/proj/.git",
           :oneline true,
           :graph true,
           :git.log/oneline true,
           :git.log/graph true},
          :errors [],
          :handler log,
          :arguments {}}
         (cliff/parse-args ["--git-dir=/other/proj/.git" "log" "--oneline" "--graph"] git)))
  (is (= {:commands ["git" "worktree" "add"],
          :options
          {:git-dir "/other/proj/.git",
           :git/git-dir "/other/proj/.git",
           :b "foo",
           :git.worktree.add/b "foo"},
          :errors [],
          :handler worktree-add
          :arguments {}}
         (cliff/parse-args ["--git-dir=/other/proj/.git" "worktree" "add" "-b" "foo"] git)))
  (is (= {:commands ["git" "worktree" "move"],
          :options
          {:git-dir "/other/proj/.git", :git/git-dir "/other/proj/.git"},
          :errors [],
          :handler worktree-move,
          :arguments {:worktree "foo", :new-path "bar"}}
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
