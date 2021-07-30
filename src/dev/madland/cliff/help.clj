(ns dev.madland.cliff.help
  (:require [clojure.tools.cli :as cli]
            [dev.madland.cliff.utils :as utils]
            [dev.madland.cliff.middleware :as mware]
            [dev.madland.cliff.vendor.tools-cli :as cli*]
            [clojure.string :as str]))

(defn help [{:dev.madland.cliff/keys [commands cli] :as ctx}]
  (let [{:keys [opts args desc doc]} (utils/get-props cli commands)

        ;; FIXME: The ordering is not guaranteed.
        next-commands (-> (utils/cli->commands-map cli)
                          (get-in commands)
                          keys
                          not-empty)

        opts? (boolean (seq opts))]
    (str/join "\n"
              ["Usage:"
               (str/join " " (-> commands
                                 (utils/conj-some (when opts? "[OPTS]"))
                                 (utils/conj-some (when next-commands
                                                    "<command> <args>"))))
               (when desc
                 (str/join "\n" ["" desc]))

               (when doc
                 (str/join "\n" ["" doc]))

               ""
               "Options: "
               (cli/summarize (cli*/compile-option-specs opts))
               (when next-commands
                 (str/join "\n"
                           [""
                            "Subcommands:"
                            (str/join "\n" next-commands)
                            ""
                            (str "Run "
                                 (str/join " " commands)
                                 " <command> --help for more details.")]))])))

(defn wrap-help [_handler]
  (fn [ctx]
    (println (help ctx))))

(def help-opt
  ["-h" "--help" "Print help text for current command."
   :middleware wrap-help])

(defn add-help-opt [props]
  (update props :opts (fnil utils/add-opt []) help-opt))

(defn add-help [cli]
  (utils/map-props cli add-help-opt))
