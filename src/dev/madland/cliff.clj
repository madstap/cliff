(ns dev.madland.cliff
  (:refer-clojure :exclude [run!])
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as str]))

(defn next-props [[props & _]]
  (when (map? props)
    props))

(defn next-command-decl [command-decl command]
  (let [command->nxt (->> command-decl
                          (filter vector?)
                          (map (juxt first rest))
                          (into {}))]
    (command->nxt command)))

(defn add-namespace [commands k]
  (keyword (str/join "." commands) (name k)))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v])) m))

(defn add-options [accumulated-opts opts commands]
  (merge accumulated-opts
         opts
         (map-keys #(add-namespace commands %) opts)))

(defn read-arguments [arguments arg-config]
  (->> (map (fn [arg {:keys [name] :as cfg}]
              [name arg])
            arguments
            arg-config)
       (into {})))

(defn assoc-some
  ([m k v]
   (cond-> m (some? v) (assoc k v)))
  ([m k v & kvs]
   (reduce (partial apply assoc-some) (assoc-some m k v) (partition 2 kvs))))

(defn parse-args* [{:keys [commands] :as ctx} args command-decl]
  (if (or (nil? args) (nil? command-decl))
    ctx
    (let [{:keys [opts handler] args-decl :args, :as props}
          (next-props command-decl)]
      (if (or (some? handler) (some? opts)) ;; Either a leaf, has opts or both.
        (let [{:keys [options arguments] :as parsed}
              (cli/parse-opts args opts :in-order true)
              new-ctx (update ctx :options add-options options commands)]
          (if (some? handler)
            (let [parsed-args (read-arguments arguments args-decl)]
              (assoc-some new-ctx :handler handler :arguments parsed-args))
            (let [[command & more-args] arguments]
              (recur (-> new-ctx (update :commands conj command))
                     more-args
                     (next-command-decl command-decl command)))))
        (let [[command & more-args] args]
          (recur (-> ctx (update :commands conj command))
                 more-args
                 (next-command-decl command-decl command)))))))

(defn parse-args [args command-decl]
  (parse-args* {:commands []
                :options {}
                :errors []}
               (cons (first command-decl) args)
               [command-decl]))

(defn run! [args command-decl]
  (let [{:keys [handler] :as ctx} (parse-args args command-decl)]
    (handler ctx)))

(defn bb-run! [command-decl]
  (when (= *file* (System/getProperty "babashka.file"))
    (run! *command-line-args* command-decl)))
