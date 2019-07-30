(ns crypto.caesar
  (:require [clojure.test :refer :all]))

(def begin-val (int \A))
(def end-val (int \Z))

(defn offset-mapping
  [offset]
  (fn [chr]
    (let [int-val (int chr)
          base-offset (cond
                        (<= 65 int-val 90) 65
                        (<= 97 int-val 122) 97
                        :else 0)
          normal-val (-> int-val (- base-offset))]
      (if (<= 0 normal-val 25)
        (-> normal-val
            (+ offset)
            (mod 26)
            (+ base-offset)
            char)
        chr))))


(defn encode
  [input offset]
  (->> input
       (transduce
         (map (offset-mapping offset))
         str)))

(defn decode
  [input offset]
  (encode input (- offset)))

(deftest sometest
  (is (= (encode "ABC" 1) "BCD"))
  (is (= (encode "AfterNoon Delight" 1)"BgufsOppo Efmjhiu"))
  (is (= (encode "A B C" 2) "C D E"))
  (is (= (encode "a b c 1234" 2) "c d e 1234"))
  (is (= (decode (encode "aBc 123 Testing" 1) 1) "aBc 123 Testing")))
