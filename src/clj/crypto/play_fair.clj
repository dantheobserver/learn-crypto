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

(defn matrix-get
  "Fetch item from matrix index."
  [m [r c]]
  (nth (nth m r) c))

(defn matrix-lookup
  "Build a cache of Character > matrix index."
  [matrix] 
  (let [idxs (for [x (range 0 5)
                   y (range 0 5)]
               [x y])]
    (transduce
      (map #(vector (matrix-get matrix %) %))
      conj
      {}
      idxs)))

(defn encoded-pair [dir-fn matrix matrix-lookup]
  (let [m-gt #(matrix-get matrix %)]
    (fn [pair]
      (let [[[ar ac] [br bc]] (map matrix-lookup pair)]
        (cond
          (= ar br) (list (m-gt [ar (mod (dir-fn ac) 5)])
                          (m-gt [br (mod (dir-fn bc) 5)]))
          (= ac bc) (list (m-gt [(mod (dir-fn ar) 5) ac])
                          (m-gt [(mod (dir-fn br) 5) bc]))
          :else (list (m-gt [ar bc])
                      (m-gt [br ac])))))))

(defn encode
  "Given a message and key, encode using the plaifair algorithm."
  ([message key] (encode message key inc))
  ([message key dir-fn]
   (let [message-digraph (digraph message)
         matrix (key-matrix (key-col key))
         matrix-lookup (matrix-lookup matrix)]
     (->> message-digraph
          (map (encoded-pair dir-fn matrix matrix-lookup))
          (map #(apply str %))
          (str/join " ")))))

(defn decode
  "Given an encoded message and key, decode"
  [encoded-msg key]
  (-> encoded-msg
      (encode key dec)
      (str/replace " " "")))


(deftest play-fair
  (testing 'digraph
    (is (= '((\H \I)) (digraph "hi")))
    (is (= '((\H \I)) (digraph "HI")))
    (is (= '((\H \I) (\M \Z)) (digraph "HIM")))
    (is (= '((\H \I) (\D \E) (\T \H) (\E \G) (\O \L) (\D \Z)) (digraph "hide the gold"))))
  (testing 'key-col
    (is (= "HELOWRD" (apply str (key-col "hello world"))))
    (is (= "HELOWRD" (apply str (key-col "HELLO WORLD")))))
  (testing 'key-matrix
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
  (testing 'matrix-lookup
    (let [matrix '((\H \E \L \O \W)
                   (\R \D \A \B \C)
                   (\F \G \I \J \K)
                   (\M \N \P \Q \S)
                   (\T \U \V \X \Y))]
      (is (= {\A [1 2] \B [1 3] \C [1 4] \D [1 1] \E [0 1]
              \F [2 0] \G [2 1] \H [0 0] \I [2 2] \J [2 3]
              \K [2 4] \L [0 2] \M [3 0] \N [3 1] \O [0 3]
              \P [3 2] \Q [3 3] \R [1 0] \S [3 4] \T [4 0]
              \U [4 1] \V [4 2] \W [0 4] \X [4 3] \Y [4 4]}
             (matrix-lookup matrix)))))
  (testing 'encoded-pair
    (let [matrix '((\H \E \L \O \W)
                   (\R \D \A \B \C)
                   (\F \G \I \J \K)
                   (\M \N \P \Q \S)
                   (\T \U \V \X \Y))
          cache  (matrix-lookup matrix)
          encoding-fn (encoded-pair inc matrix cache)]
      (testing "Row without wrapping"
        (is (= '(\E \W) (encoding-fn '(\H \O)))))
      (testing "Row with wrapping"
        (is (= '(\J \F) (encoding-fn '(\I \K)))))
      (testing "Column with wrapping"
        (is (= '(\I \V) (encoding-fn '(\A \P)))))
      (testing "Column with wrapping"
        (is (= '(\O \Q) (encoding-fn '(\X \J)))))
      (testing "Rectangle"
        (is (= '(\A \T) (encoding-fn '(\R \V)))))))
  (testing 'encoding
    (is (= "LF GD MW DN WO CV" (encode "hide the gold" "hello world"))))
  (testing 'decoding
    (is (= "HIDETHEGOLDZ" (decode "LF GD MW DN WO CV" "hello world")))))



