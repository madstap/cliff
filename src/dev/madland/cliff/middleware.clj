(ns dev.madland.cliff.middleware
  (:refer-clojure :exclude [peek])
  (:require [dev.madland.cliff.utils :as utils]))

(defn peek
  [{:keys [enter leave] :or {enter identity, leave identity}}]
  (fn [handler]
    (fn [ctx]
      (-> ctx (doto enter) handler (doto leave)))))

(defn prepend [v x]
  (vec (cons x v)))

(defn invoke-middleware [handler middleware]
  (reduce #(%2 %1) handler (reverse middleware)))

(defn concat-middleware [cli]
  (utils/walk-props
   cli
   (fn [{:keys [middleware]} child]
     (update child :middleware #(vec (concat middleware %))))))

(defn apply-middleware [command-decl]
  (-> (concat-middleware command-decl)
      (utils/map-props
       (fn [{:keys [handler middleware] :as props}]
         (cond-> props
           (some? handler)
           (update :handler invoke-middleware middleware))))))

(defmulti run-fx! (fn [fx _] fx))

(defmethod run-fx! :println [_ s] (println s))

(defmacro with-err [& body]
  `(binding [*out* *err*]
     ~@body))

(defmethod run-fx! :err/println [_ s]
  (with-err (run-fx! :println)))

(defmethod run-fx! :print-lines [_ coll]
  (run! println coll))

(defmethod run-fx! :pprint [_ s]
  (pp/pprint s))

(defmethod run-fx! :err/print-lines [_ coll]
  (with-err (run-fx! :print-lines coll)))

(defmethod run-fx! :prn [_ s] (prn s))

(defmethod run-fx! :err/prn  [_ s]
  (with-err (run-fx! :prn s)))

(defmethod run-fx! :pr-lines [_ coll]
  (run! prn coll))

(defmethod run-fx! :err/pr-lines  [_ s]
  (with-err (run-fx! :pr-lines s)))

(defmethod run-fx! :fx [_ m]
  (run! (partial apply run-fx!) m))

(defn fx-middleware [fx]
  (peek {:leave #(run-fx! fx %)}))

(defn add-fx-middleware [command-decl]
  (utils/map-props command-decl
                   (fn [{:keys [fx] :as props}]
                     (let [middleware (cond (coll? fx)
                                            (map fx-middleware fx)

                                            (keyword? fx)
                                            [(fx-middleware fx)])]
                       (update props :middleware #(vec (concat middleware %)))))))

(comment

  (run-fx! :fx {:prn "foo" :err/print-lines ["foo" "bar"]})

  (defn middleware-n [n]
    (fn [handler]
      (fn [ctx]
        (-> ctx
            (conj [:entering n])
            handler
            (conj [:leaving n])))))

  (= [[:entering 0] [:entering 1] :handled [:leaving 1] [:leaving 0]]
     ((invoke-middleware #(conj % :handled)
                         (map middleware-n (range 2)))
      []))

  (apply-middleware command-decl)

  )
