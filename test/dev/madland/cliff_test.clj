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
  (is (= {:dev.madland.cliff/commands ["git" "log"]
          :dev.madland.cliff/parsed-options
          [{:git-dir "/other/proj/.git" :dev.madland.cliff/commands ["git"]}
           {:oneline true
            :graph true
            :dev.madland.cliff/commands ["git" "log"]}]
          :dev.madland.cliff/errors nil
          :dev.madland.cliff/handler log
          :git-dir "/other/proj/.git"
          :oneline true
          :graph true}
         (cliff/parse-args ["--git-dir=/other/proj/.git" "log" "--oneline" "--graph"] git)))
  (is (= {:dev.madland.cliff/commands ["git" "worktree" "add"]
          :dev.madland.cliff/parsed-options
          [{:git-dir "/other/proj/.git" :dev.madland.cliff/commands ["git"]}
           {:branch "foo"
            :dev.madland.cliff/commands ["git" "worktree" "add"]}]
          :dev.madland.cliff/errors nil
          :dev.madland.cliff/handler worktree-add
          :git-dir "/other/proj/.git"
          :branch "foo"}
         (cliff/parse-args ["--git-dir=/other/proj/.git" "worktree" "add" "-b" "foo"] git)))
  (is (= {:dev.madland.cliff/commands ["git" "worktree" "move"]
          :dev.madland.cliff/parsed-options
          [{:git-dir "/other/proj/.git" :dev.madland.cliff/commands ["git"]}]
          :dev.madland.cliff/errors nil
          :dev.madland.cliff/arguments
          {:worktree "foo"
           :new-path "bar"
           :dev.madland.cliff/commands ["git" "worktree" "move"]}
          :dev.madland.cliff/handler worktree-move
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
