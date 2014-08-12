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
