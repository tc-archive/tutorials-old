(defn inter [xs ys]
  (reduce (fn [res x] (if (some #(= x %) ys) (conj res x) res)) #{} xs)
  )
