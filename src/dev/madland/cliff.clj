(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [dev.madland.cliff.utils :as utils]
            [dev.madland.cliff.types :as types]
            [dev.madland.cliff.vendor.tools-cli :as cli*]
            [dev.madland.cliff.middleware :as mware]
            [babashka.fs :as fs]
            [selmer.parser :as selmer]))

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

(defn flatten-cli
  [cli]
  (letfn [(step [acc commands [command & [?conf :as cli]]]
            (let [conf (when (map? ?conf) ?conf)
                  new-commands (conj commands command)
                  new-acc (assoc acc new-commands conf)]
              (->> (filter vector? cli)
                   (map #(step new-acc new-commands %))
                   (reduce merge new-acc))))]
    (step {} [] cli)))

(defn recursive-concat-env [cli]
  (utils/walk-props cli (fn [{:keys [env]} child]
                          (update child :env #(vec (concat env %))))))

(comment

  (recursive-concat-env ["foo" {:env [{:id :foo}]}
                         ["bar"
                          ["baz" {:env [{:id :baz}]}]]])

  )

(defn compile-cli [cli]
  (-> cli
      recursive-concat-env
      flatten-cli))

(defn next-commands [commands commands->opts]
  (->> (keys commands->opts)
       (filter #(= commands (butlast %)))
       (map last)
       set
       not-empty))

;; Do the initial parsing, collecting all the strings
(defn parse-args-1
  [arguments [app-name :as cli]]
  (let [commands->opts (compile-cli cli)]
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

(defn parse-and-validate-all [parsed cli]
  (let [commands->opts (compile-cli cli)]
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
  "Reduce arguments sequence into [type & data] vectors.

  The shape of data depends on the type:

  Type                  | Shape
  -------------------------------------------
  :short-opt, :long-opt | [type opt ?optarg?]
  :command              | [type command]
  :arguments            | [type & arguments]

  The first command is taken from the cli spec since it isn't passed on the
  command line.

  The :arguments vector always comes last, if present."
  [args cli]
  (let [[args explicit-args] (split-- args)
        commands->props (compile-cli cli)]
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
      [(or (butlast words) ()) (or (last words) "")])))

(defn sh-fn-name [command-name]
  (-> command-name
      munge
      (str/replace #"\." "__dot__")
      (as-> s (str "_" s "_completions"))))

(def default-completion-command
  "completions")

;; https://stackoverflow.com/questions/2339246/add-spaces-to-the-end-of-some-bash-autocomplete-options-but-not-to-others/66151065#66151065

(def bash-template
  "function {{fn-name}}()
{
    export COMP_LINE=${COMP_LINE}
    export COMP_POINT=$COMP_POINT

    RESPONSE=($(${COMP_WORDS[0]} {{completion-command}} complete bash))

    if [ $RESPONSE = 'next' ]; then
        compopt +o nospace
    fi

    unset RESPONSE[0]

    COMPREPLY=(${RESPONSE[@]})
}
complete -o nospace -F {{fn-name}} {{command-name}}")

(defn bash-script [[command-name opts]]
  (let [completion-command (or (and (map? opts)
                                    (get opts :completions))
                               default-completion-command)]
    (selmer/render bash-template {:command-name command-name
                                  :fn-name (sh-fn-name command-name)
                                  :completion-command completion-command})))

(defn script [shell [command-name :as cli]]
  (case shell
    :bash (bash-script cli)))

(defn filter-prefix [prefix completions]
  (cond->> completions
    (not (str/blank? prefix))
    (filter #(and (-> (cond-> % (map? %) :candidate)
                      (str/starts-with? prefix))
                  (not= % prefix)))))

(defn drop-upto-last-command [tokens]
  (->> tokens
       reverse
       (take-while #(not= :command (first %)))
       reverse))

(defn dbg [x]
  (spit "dbg.edn" (str (pr-str x) "\n") :append true))

;; TODO: Complete --long-opts= with arguments like this (with the =).
;;       Because it gives the user more information, ie. whether
;;       the option has an argument or not.

;; TODO: It only makes sense to specify an option multiple times if it
;;       has either :assoc-fn or :update-fn. If that is not the case we
;;       shouldn't suggest options that have already been specified.

;; TODO: Special case --help. If we have already specified any options,
;;       don't suggest help anymore. Same for --version.
;;       What's a good name for this?
:complete/standalone true
:complete/exclusive true

(defn completions [line idx cli]
  (let [[arguments word] (args-and-word line idx)
        tokens (tokenize-args arguments cli)
        commands (tokens->commands tokens)
        current-tokens (drop-upto-last-command tokens)
        [l-type l-opt l-arg] (last current-tokens)
        commands->props (compile-cli cli)
        {:keys [opts args] :as props} (commands->props commands)
        compiled-opts (cli*/compile-option-specs opts)
        opt-str->opt (merge (utils/index-by :short-opt compiled-opts)
                            (utils/index-by :long-opt compiled-opts))
        long-opts (keep (fn [{:keys [long-opt required]}]
                          (when long-opt
                            (if required
                              {:candidate (str long-opt "=")
                               :on-complete :continue}
                              {:candidate long-opt
                               :on-complete :next})))
                       compiled-opts)
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

               ;; (str/starts-with? word "-")
               ;; all-opts

               ;; Find out which arg slot we're at (if there are possible args
               ;; at this comamnd.) Concat the arg suggestions also.

               :else
               (map (fn [cmd]
                      {:candidate cmd
                       :on-complete :next})
                    possible-commands))
         (map #(if (map? %)
                 %
                 {:candidate %
                  :on-complete :next}))
         (filter-prefix word))))

(defn render-bash-completions [completions]
  (let [words (map #(cond-> % (map? %) :candidate) completions)
        on-complete (if (->> completions
                             (map #(if (map? %) (:on-complete %) :next))
                             (every? #{:continue}))
                      "continue"
                      "next")]
    (cons on-complete words)))

(comment

  (completions "foo bar --foo src/dev/madland/" 30 foo/cli)

  (completions "foo bar --foo src/dev/madland/cliff/vendor/" 43 foo/cli)

  (completions "zoo bar --foo ." 14
               ["foo" {}
                ["bar" {:opts [["-f" "--foo F"
                                :type :dir]]
                        :fx :pprint
                        :handler identity}]])

  (completions "zoo bar --an-enum :" 19
               foo/cli)

  ;; This seems to work here, but not in practice...
  ;; Surely some lovely bash edge-case.
  (= [":bar" ":baz"]
     (completions "zoo bar --an-enum :b" 20
                  foo/cli))

  )

(defn bash-complete-handler [{:keys [line point], ::keys [cli]}]
  #_(dbg [:comp (completions line point cli)
          :rendered (render-bash-completions (completions line point cli))])
  (render-bash-completions (completions line point cli)))

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
                    :type :int}]
             :fx :print-lines
             :handler bash-complete-handler}]]])

(defn add-completions
  [[_ opts :as cli]]
  (let [o (when (map? opts) opts)]
    (cond-> cli
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
  "Given command line arguments and a cli spec, returns a context.

  The context contains the parsed opts, args and etc keyed by their :id at
  the top level.

  It also contains some special keys namespaced with dev.madland.cliff
  which are considered an implementation detail unless documented below.

  Key                | Content
  -------------------|---------------------------------
  ::cliff/commands   | A vector of the commands in `arguments`

  "
  ([arguments cli]
   (parse-args arguments cli (System/getenv)))
  ([arguments cli env-vars]
   (let [prepped (-> cli
                     add-completions
                     mware/add-fx-middleware
                     mware/apply-middleware)]
     (-> (parse-args-1 arguments prepped)
         (fetch-env env-vars)
         (parse-and-validate-all prepped)
         merge-config-to-top-level
         (assoc ::cli prepped)))))

(defn run!
  "Takes a sequence of command line arguments and a cli spec and runs the
  correct handler with the context as returned by parse-args as the argument.
  Returns nil."
  [args [_ global-props :as cli]]
  (let [{::keys [handler] :as ctx} (parse-args args cli)]
    (handler ctx)
    nil))

(defn bb!
  "Takes a cli spec and calls run! on *command-line-args* and the cli spec
  iff the current file is the one invoked by babashka.

  Like the python `if __name__ == '__main__'` boilerplate, this means
  that other code can require code from the script without running
  it. It also means that we can open a repl without running the script."
  [cli]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* cli)))

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
