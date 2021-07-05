(ns dev.madland.matcher-combinators)

(defn match? [expected actual]
  (cond (and (map? expected) (map? actual))
        (every? (fn [k]
                  (let [ve (get expected k)
                        va (get actual k)]
                    (match? ve va)))
                (keys expected))

        (and (vector? expected) (vector? actual))
        (and (= (count expected) (count actual))
             (->> (map vector expected actual)
                  (every? (partial apply match?))))

        (fn? expected)
        (expected actual)

        :else (= expected actual)))
