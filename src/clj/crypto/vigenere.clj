(ns crypto.vigenere
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [crypto.common :as common]
            [clojure.string :as str]))

(defn norm-key
  [key input]
  (let [key-count (count key)
        input-count (count input)]
    (cond 
      (= key-count input-count) key
      (< key-count input-count) (apply str (take input-count (cycle key)))
      :else (apply str (take input-count key)))))

(defn encode-char [in-char key-char]
  (common/offset-char in-char (common/abs-char-val key-char)))

(defn decode-char [in-char key-char]
  (common/offset-char in-char (- (common/abs-char-val key-char))))

(defn encode [input cipher-key]
  (let [normalized-key (norm-key cipher-key input)]
    (apply str (map encode-char input normalized-key))))

(defn decode [input cipher-key]
  (let [normalized-key (norm-key cipher-key input)]
    (apply str (map decode-char input normalized-key))))

(deftest vigenere
  (testing "fill-key"
    (is (= "KEYKEYKEY" (norm-key "KEY" "TESTING12")))
    (is (= "KEYKEYKEY" (norm-key "KEYKEYKEY" "TESTING12")))
    (is (= "KEYKEYKEY" (norm-key "KEYKEYKEYKEY" "TESTING12"))))
  (testing "encode"
    (is (= "LXFOPVEFRNHR" (encode "ATTACKATDAWN" "LEMONLEMONLE")))
    (is (= "ATTACKATDAWN" (decode "LXFOPVEFRNHR" "LEMONLEMONLE")))))
