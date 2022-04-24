(ns dev.madland.cliff.interceptors)

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn enqueue
  [ctx interceptors]
  (update ctx ::queue (fnil into empty-queue) interceptors))

(defn enter [{::keys [queue] :as ctx}]
  (if (empty? queue)
    ctx
    (let [{:keys [enter] :or {enter identity} :as interceptor} (peek queue)]
      (recur (-> ctx
                 (update ::queue pop)
                 (update ::stack conj interceptor)
                 enter)))))

(defn leave [{::keys [stack] :as  ctx}]
  (if (empty? stack)
    ctx
    (let [{:keys [leave] :or {leave identity}} (peek stack)]
      (recur (-> ctx
                 (update ::stack pop)
                 leave)))))

(defn execute
  ([ctx]
   (-> ctx enter leave (dissoc ::stack ::queue)))
  ([ctx interceptors]
   (execute (enqueue ctx interceptors))))

(defn handler [f & args]
  {:enter (fn [{:keys [params] :as ctx}]
            (assoc ctx :result (apply f params args)))})

(comment

  (execute {:params {:foo 4}}
           [{:leave #(update % :bar str)}
            {:enter #(assoc % :bar 2)}
            {:enter #(update % :bar inc)}
            (handler #(repeat (:foo %) :x))])

  )
