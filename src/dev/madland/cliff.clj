(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]))

(defn read-arguments [arguments arg-config]
  (->> (map (fn [arg {:keys [id] :as cfg}]
              [id arg])
            arguments
            arg-config)
       (into {})))


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

(defn next-commands [commands commands->opts]
  (->> (keys commands->opts)
       (filter #(= commands (butlast %)))
       (map last)
       set
       not-empty))

(defn conj-some [coll & xs]
  (apply conj coll (remove nil? xs)))

(defn parse-args-1
  [arguments [app-name :as command-decl]]
  (let [commands->opts (flatten-command-decl command-decl)]
    (loop [{::keys [commands], :as ctx} {::commands [app-name]
                                         ::parsed-options []
                                         ::errors nil}
           arguments arguments]
      (let [{:keys [opts handler args] :as props}
            (commands->opts commands)

            {:keys [options errors] new-arguments :arguments :as parsed}
            (if opts
              (cli/parse-opts arguments opts :in-order (nil? args))
              {:arguments arguments})

            new-ctx
            (-> ctx
                (update ::parsed-options conj-some
                        (some-> options not-empty (assoc ::commands commands))))]

        (cond (and (nil? handler)
                   (empty? new-arguments))
              (update new-ctx ::errors conj "Insufficient input")

              (some? args)
              ;; TODO: Error handling here.
              (let [parsed-args (-> (read-arguments new-arguments args)
                                    (assoc ::commands commands))]
                (-> new-ctx
                    (assoc ::arguments parsed-args ::handler handler)))

              :else
              (if-some [nxt (next-commands commands commands->opts)]
                (let [[command & more-args] new-arguments]
                  (if (contains? nxt command)
                    (recur (update new-ctx ::commands conj command)
                           more-args)
                    (do (prn nxt new-arguments)
                        (update new-ctx ::errors conj
                                (str "Unknown command " command)))))
                (if (some? handler)
                  (assoc new-ctx ::handler handler)
                  (update new-ctx ::errors (fnil conj [])))))))))

(defn prep-parsed-opts [parsed-options]
  (transduce (map #(dissoc % ::commands)) merge parsed-options))

(defn prep-parsed-args [parsed-args]
  (dissoc parsed-args ::commands))

(defn parse-args-2 [{::keys [commands parsed-options arguments] :as parsed}]
  (let [config (merge (prep-parsed-opts parsed-options)
                      (prep-parsed-args arguments))]
    (merge parsed config)))

(defn parse-args [arguments command-decl]
  (-> (parse-args-1 arguments command-decl)
      (parse-args-2)))

(defn run! [args command-decl]
  (let [{::keys [handler] :as ctx} (parse-args args command-decl)]
    (handler ctx)))

(defn bb! [command-decl]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* command-decl)))
