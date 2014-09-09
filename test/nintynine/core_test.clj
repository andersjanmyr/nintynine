(ns nintynine.core-test
  (:require [midje.sweet :refer :all]
            [clojure.math.combinatorics :as comb]))

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
             (conj (subvec %1 0 (dec (count %1)))
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

(defn encode-modified [l]
  (let [f (fn [sl]
            (let [c (count sl)]
              (if (= c 1)
                (first sl)
                [c (first sl)])))]
    (map f (pack l))))

(fact "P11: run length encoding, direct single elements"
  (encode-modified [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) =>
    [[4 \a] \b [2 \c] [2 \a] \d [4 \e]])

(defn decode [l]
  (flatten (map #(repeat (first %1) (second %1)) l)))

(fact "P12: run length decoding of list"
  (decode [[4 \a] [1 \b] [2 \c] [2 \a] [1 \d] [4 \e]]) =>
    [\a \a \a \a \b \c \c \a \a \d \e \e \e \e])

(defn encode-direct [l]
  (let [r #(let [[n c] (last %1)]
             (if (= c %2)
               (update-in %1 [(dec (count %1)) 0] inc)
               (conj %1 [1 %2])))]
    (reduce r [] l)))

(fact "P13: direct run length encoding of list"
  (encode-direct [\a \a \a \a \b \c \c \a \a \d \e \e \e \e]) =>
  [[4 \a] [1 \b] [2 \c] [2 \a] [1 \d] [4 \e]])

(defn duplicate [l]
  (mapcat (fn [i] [i i]) l))

(fact "P14: duplicate element of list "
  (duplicate [1 2 3 4]) => [1 1 2 2 3 3 4 4])

(defn duplicate-n [n l]
  (mapcat (fn [i] (repeat n i)) l))

(fact "P15: n-plicate element of list "
  (duplicate-n 3 [1 2 3 4]) => [1 1 1 2 2 2 3 3 3 4 4 4])

(defn drop-n [n l]
  (mapcat #(take 2 %1) (partition-all n l)))

(fact "P16: Drop every Nth element from a list."
  (drop-n 3 [1 2 3 4 5 6 7 8]) => [1 2 4 5 7 8])

(fact "P17: Split a list into two parts."
  (split-at 2 [1 2 3 4 5 6]) => [[1 2] [3 4 5 6]])

(defn slice [start end l]
  (->> l (drop start) (take (- end start))))

(fact "P18:   Extract a slice from a list."
  (slice 2 4 [1 2 3 4 5 6]) => [3 4])

(defn rotate [n l]
  (if (< n 0)
    (let [m (+ (count l) n)]
      (concat (drop m l) (take m l)))
    (concat (drop n l) (take n l))))

(fact "P19: Rotate a list N places to the left"
  (rotate 3 [1 2 3 4 5 6]) => [4 5 6 1 2 3]
  (rotate -2 [1 2 3 4 5 6]) => [5 6 1 2 3 4])

(defn remove-at [n l]
  [(into [] (concat (subvec l 0 n) (subvec l (inc n)))) (l n)])

(fact "P20: Remove k:th element from list return a tuple"
  (remove-at 2 [1 2 3 4 5 6]) => [[1 2 4 5 6] 3])

(defn insert [e i l]
  (concat (subvec l 0 i) [e] (subvec l i)))

(fact "P21: Insert an element at a given position into a list."
  (insert \a 2 [1 2 3 4]) => [1 2 \a 3 4])

(fact "P22: Create a list containing all integers within a given range."
  (range 5 8) => [5 6 7])

(defn random-select [n l]
  (loop [els l
         result []]
    (if (= (count result) n)
      result
      (let [[new-els el] (remove-at (rand-int (count els)) els)]
          (recur new-els (conj result el))))))

(fact "P23: Extract a given number of randomly selected elements from a list."
  (let [nums (random-select 5 [1 2 3 4 5])]
    (do
      (count nums) => 5)
      (count nums) => (count (set nums))
      (every? #(>= %1 1) nums)
      (every? #(<= %1 5) nums)))

(defn lotto [n m]
  (random-select n (into [] (range 1 (inc m)))))


(fact "P24: Lotto: Draw N different random numbers from the set 1..M."
  (let [nums (lotto 7 36)]
    (do
      (count nums) => 7
      (count nums) => (count (set nums))
      (every? #(>= %1 0) nums) => true
      (every? #(>= %1 0) nums) => true
      (every? #(<= %1 35) nums) => true)))

(defn random-permute [l]
  (random-select (count l) l))

(fact "P25: Generate a random permutation of the elements of a list."
  (let [perm (random-permute (into [] (range 0 10)))]
    (do
      (count perm) => 10
      (count perm) => (count (set perm))
      (every? #(>= %1 0) perm) => true
      (every? #(<= %1 10) perm) => true)))

(defn combinations [n l]
  (comb/combinations l n))

(fact "P26: Generate the combinations of K distinct objects chosen from the N elements of a list."
  (let [comb (combinations 3 (into [] (range 0 12)))]
    (do
      (count comb) => 220
      (every? #(<= (count %1) 3) comb) => true)))

