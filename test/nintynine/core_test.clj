(ns nintynine.core-test
  (:require [midje.sweet :refer :all]))

(fact "P01: last element"
  (last (list 1 2 3)) => 3
  (last [1 2 3]) => 3
  )

(fact "P02: last but one"
  (last (butlast (list 1 2 3))) => 2
  (-> (list 1 2 3) butlast last) => 2
  (-> [1 2 3] butlast last) => 2)

(fact "P03: nth element"
  (nth '(1 2 3) 0) => 1
  (nth [1 2 3] 0) => 1)

(fact "P04: number of elements"
  (count '(1 2 3)) => 3)

(fact "P05: reversed"
  (reverse '(1 2 3)) => '(3 2 1)
  (reverse [1 2 3]) => [3 2 1])

(defn palindrome [l]
  (= (reverse l) l))

(fact "P06: palindrome"
  (palindrome [1 2 3 2 1]) => true
  (palindrome [1 2 3 2 3]) => false)

(fact "P07: flatten"
  (flatten [1 [2 3] [4 5]]) => [1 2 3 4 5])

(defn compress [l]
  (let [c #(if (= (last %1) %2) %1 (conj %1 %2))]
    (reduce c [] l)))


(fact "P08: compress consecutive duplicates"
  (compress [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) => [ \a \b \c \a \d \e])

(defn pack [l]
  (let [c #(if (= (last (last %1)) %2)
             (conj (subvec %1 0 (- (count %1) 1))
                   (conj (last %1) %2))
             (conj %1 [%2]))]
    (reduce c [] l)))

(fact "P09: pack consecutive duplicates"
  (pack [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) =>
    [[\a \a \a \a] [\b] [\c \c] [\a \a] [\d] [\e \e \e \e]])

(defn encode [l]
  (map (fn [sl] [(count sl) (first sl)]) (pack l)))

(fact "P10: run length encoding of list"
  (encode [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) =>
    [[4 \a] [1 \b] [2 \c] [2 \a] [1 \d] [4 \e]])

(defn encodeModified [l]
  (let [f (fn [sl]
            (let [c (count sl)]
              (if (= c 1)
                (first sl)
                [c (first sl)])))]
    (map f (pack l))))

(fact "P11: run length encoding, direct single elements"
  (encodeModified [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) =>
    [[4 \a] \b [2 \c] [2 \a] \d [4 \e]])

(defn decode [l]
  (flatten (map #(repeat (first %1) (second %1)) l)))

(fact "P12: run length decoding of list"
  (decode [[4 \a] [1 \b] [2 \c] [2 \a] [1 \d] [4 \e]]) =>
    [\a \a \a \a \b \c \c \a \a \d \e \e \e \e])

(defn encodeDirect [l]
  (let [r #(let [[n c] (last %1)]
             (if (= c %2)
               (conj (subvec %1 0 (- (count %1) 1))
                     [(+ n 1) %2])
               (conj %1 [1 %2])))]
    (reduce r [] l)))

(fact "P13: direct run length encoding of list"
  (encodeDirect [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) =>
  [[4 \a] [1 \b] [2 \c] [2 \a] [1 \d] [4 \e]])

