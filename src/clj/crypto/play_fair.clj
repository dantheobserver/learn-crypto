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

(defn matrix-cache
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

(defn encoded-pair [matrix-lookup]
  (fn [pair]
    ))

(defn encode
  "Given a message and key, encode using the plaifair algorithm."
  [message key]
  (let [message-digraph (digraph message)
        matrix (key-matrix (key-col key))
        matrix-lookup (matrix-cache matrix)]
    (map (encoded-pair matrix-lookup)
         message-digraph)
    ;;For each pair in message-digraph
    ;;1 find position of each letter in pair
    ;; if in same row, get sequence of chars in row and use going right, wrapping if necessary
    ;; if in same column, get sequence of chars going down, wrapping if necessary
    ;; otherwise, get rectangle with letters, and choose opposite corner characters
    ;; return encoded string
    ))


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
  (testing 'matrix-cache
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
             (matrix-cache matrix)))))
  (testing encoded-pair
    (let [matrix '((\H \E \L \O \W)
                   (\R \D \A \B \C)
                   (\F \G \I \J \K)
                   (\M \N \P \Q \S)
                   (\T \U \V \X \Y))
          cache  (matrix-cache matrix)
          encoding-fn (encoded-pair cache)]
      (is (= '(\E \W) (encoding-fn '(\H \O)))) ;; Row no-wraping
      (is (= '(\J \F) (encoding-fn '(\I \K)))) ;; Row wraping
      (is (= '(\I \V) (encoding-fn '(\A \P)))) ;; Column no-wraping
      (is (= '(\O \Q) (encoding-fn '(\X \J)))) ;; Column wraping
      (is (= '(\B \T) (encoding-fn '(\R \V)))) ;; Rectangle
      ))
  )

