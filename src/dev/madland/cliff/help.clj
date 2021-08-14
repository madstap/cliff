(ns dev.madland.cliff.help
  (:require [clojure.tools.cli :as cli]
            [dev.madland.cliff.utils :as utils]
            [dev.madland.cliff.middleware :as mware]
            [dev.madland.cliff.vendor.tools-cli :as cli*]
            [clojure.string :as str]))

;; https://stackoverflow.com/questions/9725675/is-there-a-standard-format-for-command-line-shell-help-text
;; http://docopt.org/

(defn options [opts]
  (when-not (empty? opts)
    (utils/text
     ["Options:"
      (cli/summarize (cli*/compile-option-specs opts))])))

(defn argument [{:keys [optional varargs id]}]
  (cond-> (str "<" (name id) ">")
    varargs (as-> $ (str $ "..."))
    optional (as-> $ (str "[" $ "]"))))

(defn usage [commands next-commands {:keys [opts args]}]
  (utils/text
   ["Usage:"
    (when next-commands
      (str/join " " (-> commands
                        (utils/conj-some (when-not (empty? opts) "[OPTS]"))
                        (utils/conj-some (when next-commands
                                           "<command> <args>")))))
    (when args
      (str/join " " (concat commands (map argument args))))]))


(defn description [desc doc]
  (utils/text
   [(when desc
      (utils/text desc))

    (when (and desc doc)
      "")

    (when doc
      (utils/text doc))]))

(defn find-subcommands [{:dev.madland.cliff/keys [commands cli] :as ctx}]
  (letfn [(step [current-commands [command _ & more]]
            (let [new-commands (conj current-commands command)]
              (if (= new-commands commands)
                (map (fn [[cmd props]]
                       (assoc props :command cmd))
                     more)
                (some (partial step new-commands) more))))]
    (not-empty (step [] (utils/normalize cli)))))

(comment

  (find-subcommands #:dev.madland.cliff{:commands ["foo" "bar"]
                                        :cli ["foo"
                                              ["bar" {:opts []}
                                               ["quux" {:opts []}]
                                               ["asdasd"]]
                                              ["baz"]]})

  )

(defn subcommands-list [cmds]
  (let [spaces (+ (apply max (map #(count (:command %)) cmds)) 2)]
    (->> cmds
         (map (fn [{:keys [command desc]}]
                (str (utils/right-pad command spaces \space) desc)))
         utils/text)))

(defn subcommands [{:dev.madland.cliff/keys [commands] :as ctx}]
  (when-some [cmds (find-subcommands ctx)]
    (utils/text
     [""
      "Subcommands:"
      (subcommands-list cmds)
      ""
      (str "Run "
           (str/join " " commands)
           " <command> --help for more details.")])))

(defn help [{:dev.madland.cliff/keys [commands cli] :as ctx}]
  (let [{:keys [opts args desc doc] :as props} (utils/get-props cli commands)

        ;; FIXME: The ordering is not guaranteed.
        next-commands (-> (utils/cli->commands-map cli)
                          (get-in commands)
                          keys
                          not-empty)

        opts? (boolean (seq opts))]
    (utils/text
     [(description desc doc)

      ""

      (usage commands next-commands props)

      ""

      (options opts)

      ""

      (subcommands ctx)])))

(defn wrap-help [_handler]
  (fn [ctx]
    (println (help ctx))))

(def help-opt
  ["-?" "--help" "Print help text for current command."
   :middleware wrap-help])

(defn add-help-opt [props]
  (update props :opts (fnil utils/add-opt []) help-opt))

(defn add-help [cli]
  (utils/map-props cli add-help-opt))
