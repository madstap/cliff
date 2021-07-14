(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [dev.madland.cliff.utils :as utils]
            [dev.madland.cliff.types :as types]
            [dev.madland.cliff.vendor.tools-cli :as cli*]
            [dev.madland.cliff.middleware :as mware]
            [babashka.fs :as fs]))

(defn read-arguments [arguments arg-config]
  ;; TODO: Error handling
  (let [[normal-args [{vararg-id :id}]]
        (if (:varargs (first arg-config))
          [[] arg-config]
          (partition-by :varargs arg-config))

        normal-parsed (->> (map (fn [arg {:keys [id] :as cfg}]
                                  [id arg])
                                arguments
                                normal-args)
                           (into {}))
        vararg-vals (vec (drop (count normal-args) arguments))]
    (cond-> normal-parsed
      (some? vararg-id) (assoc vararg-id vararg-vals))))

(defn flatten-command-decl
  [command-decl]
  (letfn [(step [acc commands [command & [?conf :as command-decl]]]
            (let [conf (when (map? ?conf) ?conf)
                  new-commands (conj commands command)
                  new-acc (assoc acc new-commands conf)]
              (->> (filter vector? command-decl)
                   (map #(step new-acc new-commands %))
                   (reduce merge new-acc))))]
    (step {} [] command-decl)))

(defn recursive-concat-env [command-decl]
  (utils/walk-props command-decl (fn [{:keys [env]} child]
                                   (update child :env #(vec (concat env %))))))

(comment

  (recursive-concat-env ["foo" {:env [{:id :foo}]}
                         ["bar"
                          ["baz" {:env [{:id :baz}]}]]])

  )

(defn compile-command-decl [command-decl]
  (-> command-decl
      recursive-concat-env
      flatten-command-decl))

(defn next-commands [commands commands->opts]
  (->> (keys commands->opts)
       (filter #(= commands (butlast %)))
       (map last)
       set
       not-empty))

;; Do the initial parsing, collecting all the strings
(defn parse-args-1
  [arguments [app-name :as command-decl]]
  (let [commands->opts (compile-command-decl command-decl)]
    (loop [{::keys [commands], :as ctx} {::commands [app-name]
                                         ::parsed-options {}
                                         ::errors nil}
           arguments arguments]
      (let [{:keys [opts handler args env] :as props}
            (commands->opts commands)

            id->compiled-opts (->> (cli*/compile-option-specs opts)
                                   (utils/index-by :id))

            id->arg-config (utils/index-by :id args)

            ;; TODO: error handling
            {:keys [options errors] new-arguments :arguments :as parsed}
            (if opts
              (cli/parse-opts arguments (cli*/remove-unknown-keys opts)
                              :in-order (nil? args))
              {:arguments arguments})

            new-ctx
            (-> ctx
                (update ::parsed-options merge
                        (utils/map-kv-vals (fn [k v]
                                             (merge (id->compiled-opts k)
                                                    {:id k
                                                     :value v
                                                     ::commands commands}))
                                           options)))]

        (cond (and (nil? handler)
                   (empty? new-arguments))
              (update new-ctx ::errors conj "Insufficient input")

              :else
              (let [nxt (next-commands commands commands->opts)
                    [command & more-args] new-arguments]
                (if (contains? nxt command)
                  (recur (update new-ctx ::commands conj command)
                         more-args)
                  ;; TODO: Error handling here.
                  (let [parsed-args
                        (if (nil? args)
                          nil
                          (->> (read-arguments new-arguments args)
                               (utils/map-kv-vals
                                (fn [k v]
                                  (merge (id->arg-config k)
                                         {:id k
                                          :value v
                                          ::commands commands})))))

                        new-new-ctx (utils/assoc-some new-ctx ::arguments parsed-args)
                        new-new-arguments (drop (count parsed-args)
                                                new-arguments)]

                    (-> new-new-ctx
                        (utils/assoc-some ::handler handler
                                          ::env (not-empty
                                                 (utils/index-by :id env)))
                        (cond-> (nil? handler)
                          (update ::errors (fnil conj [])
                                  (str "No handler for " commands))

                          (seq new-new-arguments)
                          (-> (assoc ::extra-input new-new-arguments)
                              (update ::errors (fnil conj [])
                                      "Extra input"))))))))))))

(defn parsed-values [parsed-options]
  (utils/map-vals :value parsed-options))

(defn collect-errors [{::keys [errors parsed-options arguments]}]
  ;; TODO:
  )

(defn parse-and-validate [parsed-values commands->opts]
  (utils/map-vals
   (fn [{:keys [type value]
         :or {type :string}
         :as parsed-val}]
     (-> (merge parsed-val
                (types/parse-and-validate types/types type value))
         (assoc :initial-value value)))
   parsed-values))

(defn parse-and-validate-all [parsed command-decl]
  (let [commands->opts (compile-command-decl command-decl)]
    (-> parsed
        (utils/update-existing ::parsed-options
                               parse-and-validate commands->opts)
        (utils/update-existing ::arguments
                               parse-and-validate commands->opts)
        (utils/update-existing ::env
                               parse-and-validate commands->opts))))

;; Get a map of the keys and values we're actually after, merge that into the
;; top level of the context.
(defn merge-config-to-top-level
  [{::keys [commands parsed-options arguments env] :as parsed}]
  (let [config (merge (parsed-values env)
                      (parsed-values parsed-options)
                      (parsed-values arguments))]
    (merge parsed config)))

(defn fetch-env [parsed env-vars]
  (update parsed ::env
          (partial utils/map-vals #(assoc % :value (get env-vars (:var %))))))

(defn cli->subcommands-map [cli]
  (letfn [(step [acc [cmd & more]]
            (assoc acc cmd (reduce step {} (filter vector? more))))]
    (reduce step {} (filter vector? cli))))

(defn cli->commands-map [cli]
  {(first cli) (cli->subcommands-map cli)})

(comment

  (cli->subcommands-map
   ["foo" {}
    ["bar" ["baz" {}]]
    ["quux" ["asd"]]])

  (cli->commands-map
   ["foo" {}
    ["bar" ["baz" {}]]
    ["quux" ["asd"]]])

  (cli->subcommands-map ["foo"])

  )

(defn props->req-set [{:keys [opts]}]
  (->> (cli*/compile-option-specs opts)
       (filter :required)
       (mapcat (juxt :short-opt :long-opt))
       (remove nil?)
       (set)))

(comment
  (props->req-set {:opts [["-f" "--foo FOO"]
                          ["-b" "--bar"]]})
  )

(defn split-- [args]
  (-> (split-with (complement #{"--"}) args)
      (update 1 next)))

(defn tokens->commands [tokens]
  (keep (fn [[t v]] (when (= :command t) v)) tokens))

(defn tokenize-args
  "Reduce arguments sequence into [type opt ?optarg?] vectors and a vector
  of remaining arguments. Returns as [tokens remaining-args].

  The first command is taken from the cli spec since it isn't passed on the
  command line.

  type is one of :short-opt, :long-opt, :command, :arguments

  Arguments always come last, if present."
  [args cli]
  (let [[args explicit-args] (split-- args)
        commands->props (compile-command-decl cli)]
    (loop [args args
           subcommands (cli->subcommands-map cli)
           tokens [[:command (first cli)]]]

      (let [commands (tokens->commands tokens)
            props (-> commands commands->props)
            required-set (-> props props->req-set)

            [ts [cmd & next-args]] (cli*/tokenize-args required-set
                                                       args
                                                       :in-order true)]

        (if-some [next-subcommands (get subcommands cmd)]
          (let [next-tokens (-> tokens (into ts) (conj [:command cmd]))]
            (recur next-args next-subcommands next-tokens))

          ;; In this case we need to run cli*/tokenize-args again
          ;; with :in-order false so we can collect opts that
          ;; are passed after the arguments.
          (let [[ts arguments] (cli*/tokenize-args required-set
                                                   args
                                                   :in-order false)]
            (-> tokens
                (into ts)
                (utils/conj-some
                 (some->> (into arguments explicit-args)
                          not-empty
                          (into [:arguments]))))))))))

(comment

  (cli*/tokenize-args #{} ["--foo"])

  (tokenize-args ["foo" "--opt" "arg" "bar"]
                 ["smthn" {}
                  ["foo" {:opts [[nil "--opt X"]]}
                   ["bar" {}]]])

  (tokenize-args ["--opt" "arg" "foo" "bar" "--" "pos-arg" "--other-arg"]
                 ["smthn" {:opts [[nil "--opt X"]]}
                  ["foo" {}
                   ["bar" {:args [{:id :xs
                                   :varargs true}]}]]])

  (tokenize-args ["foo" "--opt"]
                 ["smthn" {}
                  ["foo" {:opts [["-o" "--opt X"]]}
                   ["bar" {}]]])

  (tokenize-args ["foo" "-o"]
                 ["smthn" {}
                  ["foo" {:opts [["-o" "--opt X"]]}
                   ["bar" {}]]])


  (tokenize-args ["foo" "--opt"]
                 ["smthn" {}
                  ["foo" {:opts [[nil "--opt"]]}
                   ["bar" {}]]])

  (re-find #"\S+" "foo  bar")

  )

(defn split-words [line]
  (->> (str/split line #"\s+") (remove str/blank?)))

(comment

  (= ["foo" "bar" "baz"]
     (split-words "  foo bar   baz"))

  )

(defn args-and-word
  "GIven the line and cursor index, returns the args up to the cursor and the
  current word to be completed as a tuple [args word]."
  [line idx]
  (let [l (subs line 0 idx)
        ;; We ignore the first word as it will be the name of the executable
        ;; and might be an alias. Also that's how normal arguments are passed.
        words (rest (split-words l))]
    (if (or (re-find #"\s$" l) (= "" l))
      [words ""]
      [(or (butlast words) ()) (last words)])))

(comment

  (= [["bar"] "b"]
     (args-and-word "foo bar baz" 9))

  (= [[] "bar"]
     (args-and-word "foo bar baz" 7))

  (= [[] ""]
     (args-and-word "foo " 4))

  (args-and-word "" 0)

  )

(defn sh-fn-name [command-name]
  (-> command-name
      munge
      (str/replace #"\." "__dot__")
      (as-> s (str "_" s "_completions"))))

(def default-completion-command
  "completions")

(defn script [shell [command-name opts]]
  (let [completion-command (or (and (map? opts)
                                    (get opts :completions))
                               default-completion-command)]
    (case shell
      :bash
      (let [fn-name (sh-fn-name command-name)]
        (str "function " fn-name "() \n"
             (format "{
    export COMP_LINE=${COMP_LINE}
    export COMP_CWORD=$COMP_CWORD
    export COMP_POINT=$COMP_POINT

    COMPREPLY=($(${COMP_WORDS[0]} %s complete bash ${COMP_WORDS[@]}))
}\n"
                     completion-command)
             (format "complete -F %s %s" fn-name command-name))))))

(defn filter-prefix [prefix words]
  (cond->> words
    (not (str/blank? prefix))
    (filter #(and (str/starts-with? % prefix) (not= % prefix)))))

(defn drop-upto-last-command [tokens]
  (->> tokens
       reverse
       (take-while #(not= :command (first %)))
       reverse))

(defn dbg [x]
  (spit "dbg.edn" (str (pr-str x) "\n") :append true))

(comment

  (tokenize-args ["bar"] foo/cli)

  )

(defn completions [line idx cli]
  (let [[arguments word] (args-and-word line idx)
        tokens (tokenize-args arguments cli)
        commands (tokens->commands tokens)
        current-tokens (drop-upto-last-command tokens)
        [l-type l-opt l-arg] (last current-tokens)
        commands->props (compile-command-decl cli)
        {:keys [opts args] :as props} (commands->props commands)
        compiled-opts (cli*/compile-option-specs opts)
        opt-str->opt (merge (utils/index-by :short-opt compiled-opts)
                            (utils/index-by :long-opt compiled-opts))
        long-opts (keep :long-opt compiled-opts)
        all-opts (->>  compiled-opts
                       (mapcat (juxt :short-opt :long-opt))
                       (remove nil?))
        commands-map (cli->commands-map cli)
        possible-commands (keys (get-in commands-map commands))]

    ;; (dbg [:tokens tokens :line line :idx idx])

    (->> (cond (and (contains? #{:short-opt :long-opt} l-type)
                    (nil? l-arg)
                    (contains? (props->req-set props) l-opt))
               (types/invoke-completions types/types (opt-str->opt l-opt) word)

               (str/starts-with? word "--")
               long-opts

               (str/starts-with? word "-")
               all-opts

               ;; TODO: Args

               :else
               (concat possible-commands long-opts))
         (filter-prefix word))))

(comment

  (completions "zoo bar --foo ." 14
               ["foo" {}
                ["bar" {:opts [["-f" "--foo F"
                                :type :dir]]
                        :fx :pprint
                        :handler identity}]])


  (completions "zoo bar --an-enum " 18
               foo/cli)

  (completions "zoo bar --an-enum :" 19
               foo/cli)

  ;; This seems to work here, but not in practice...
  ;; Surely some lovely bash edge-case.
  (= [":bar" ":baz"]
     (completions "zoo bar --an-enum :b" 20
                  foo/cli))



  (= ["--foo" "--an-enum"]
     (completions "foo bar --an-enum bar --" 24
                  foo/cli))

  (= ["bar" "baz"]
     (completions "zoo bar --an-enum b" 19
                  foo/cli))




  (completions "foo " 0
               ["foo" {}
                ["bar" {:opts [["-f" "--foo F"
                                :type :file]]
                        :fx :pprint
                        :handler identity}]])

  )

(defn complete-handler [{:keys [line words cword point], ::keys [cli]}]
  (completions line point cli))

(defn completions-cli [command]
  [command
   ["script" {:args [{:id :shell
                      :type :enum
                      :values #{:bash :zsh}}]
              :fx :println
              :handler #(script (:shell %) (::cli %))}]
   ["complete"
    ["bash" {:env [{:id :line
                    :var "COMP_LINE"}
                   {:id :point
                    :var "COMP_POINT"
                    :type :int}
                   {:id :cword
                    :var "COMP_CWORD"
                    :type :int}]
             :args [{:id :words
                     :varargs true}]
             :fx :print-lines
             :handler complete-handler}]]])

(defn add-completions
  [[_ opts :as command-decl]]
  (let [o (when (map? opts) opts)]
    (cond-> command-decl
      (not= [:completions nil] (find o :completions))
      (conj (completions-cli (:completions o "completions"))))))

(comment
  (parse-args ["completions" "script" "bash"]
              (add-completions ["foo" {:completions "foobar"}
                                ["bar" {}]]))

  (parse-args ["foobar" "script" "bash"]
              (add-completions ["foo" {:completions "foobar"}
                                ["bar" {}]]))

  )

(defn parse-args
  ([arguments command-decl]
   (parse-args arguments command-decl (System/getenv)))
  ([arguments command-decl env-vars]
   (let [prepped (-> command-decl
                     add-completions
                     mware/add-fx-middleware
                     mware/apply-middleware)]
     (-> (parse-args-1 arguments prepped)
         (fetch-env env-vars)
         (parse-and-validate-all prepped)
         merge-config-to-top-level
         (assoc ::cli prepped)))))

(defn run! [args [_ global-props :as command-decl]]
  (let [{::keys [handler] :as ctx} (parse-args args command-decl)]
    (handler ctx)
    nil))

(defn bb! [command-decl]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* command-decl)))

(comment

  ;; TODO: Make these into tests
  ["foo" {}
   ["bar" {:middleware [(fn [handler]
                          (fn [ctx]
                            (prn "fooo")
                            (handler ctx)))]
           :handler prn}]]


  (run!
   ["bar"]
   ["foo" {}
    ["bar" {:fx :println
            :handler identity}]])


  )
