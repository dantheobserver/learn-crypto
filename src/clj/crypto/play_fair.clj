(ns crypto.play-fair
  (:require [crypto.common :as common]
            [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn key-col
  "Given a key to use for encryption,
  generate the distinct letters for that key
  to use when generating the graph."
  [key]
  (->> key
       str/upper-case
       distinct
       (filter #(not= \space %))))

(defn key-matrix
  "Given they distinct letters of the key, generate
  a 5x5 grid of letters to use to encode the message."
  ([key-col] (key-matrix key-col {:excluded-char \Q}))
  ([key-col {:keys [excluded-char]}]
   (let [alphabet (into #{} (map char) (range 65 91))
         diff-set (set/difference alphabet key-col #{excluded-char})
         complete-matrix (concat key-col diff-set)]
     (partition 5 complete-matrix))))

(defn digraph
  "Given a message to encrypt,
  generate pairs to use for encryption"
  [input]
  (let [input (-> input
                  str/upper-case
                  (str/replace " " ""))]
    (partition 2 (if (odd? (count input))
                   (str input "Z")
                   input))))


(defn encode [message key]
  (let [message-digraph (digraph message)
        matrix (key-matrix (key-col key))]
    ;;For each pair in message-digraph
    ;;1 find position of each letter in pair
    ;; if in same row, get sequence of chars in row and use going right, wrapping if necessary
    ;; if in same column, get sequence of chars going down, wrapping if necessary
    ;; otherwise, get rectangle with letters, and choose opposite corner characters
    ;; return encoded string))


(deftest play-fair
  (testing "digraph"
    (is (= '((\H \I)) (digraph "hi")))
    (is (= '((\H \I)) (digraph "HI")))
    (is (= '((\H \I) (\M \Z)) (digraph "HIM")))
    (is (= '((\H \I) (\D \E) (\T \H) (\E \G) (\O \L) (\D \Z)) (digraph "hide the gold"))))
  (testing "key-col"
    (is (= "HELOWRD" (apply str (key-col "hello world"))))
    (is (= "HELOWRD" (apply str (key-col "HELLO WORLD")))))
  (testing "key-matrix"
    (is (= '((\H \E \L \O \W)
             (\R \D \A \B \C)
             (\F \G \I \J \K)
             (\M \N \P \S \T)
             (\U \V \X \Y \Z))
           (key-matrix '(\H \E \L \O \W \R \D))))
    (is (= '((\H \E \L \O \W)
             (\R \D \A \B \C)
             (\F \G \I \J \K)
             (\M \N \P \Q \S)
             (\T \U \V \X \Y))
           (key-matrix '(\H \E \L \O \W \R \D)
                       {:excluded-char \Z}))))
  (testing "pair-column"
    (let [digraph '((\P \L) (\A \Y) (\F \I) (\R \E) (\X \M))]))
  
  #_(testing "encode"
      (is (= "LF GD MW DN WO CV" (encode "hide the gold" "hello world"))))
  )
#_(run-tests 'crypto.play-fair)

