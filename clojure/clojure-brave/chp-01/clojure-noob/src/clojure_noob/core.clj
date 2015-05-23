


;(= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")


(defn ip [xs x]
  (loop [in (seq xs) out []]
    (if (= 1 (count in))
      (conj out (first in))
      (recur (rest in) (conj out (first in) x)) ))
  )

