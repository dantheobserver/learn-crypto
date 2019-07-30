(ns crypto.common
  (:require [clojure.string :as str]))

(defn alpha-char? [char]
  (let [char-val (int char)]
    (<= 65 char-val 90)))

(defn abs-char-val [char]
  (-> char
      int
      (- 65)))

(defn offset-char [chr offset]
  (if (alpha-char? chr)
    (-> chr
        int
        (abs-char-val)
        (+ offset)
        (mod 26)
        (+ 65)
        char)
    chr))
