(ns gbt.core-test
  (:require [clojure.test :refer :all]
            [gbt.core :refer :all]))

;; Use GBT2
(def sp (create-space 2))

(deftest space-creation
  (testing "Creating a 2d Generalized Balanced Ternary space"
    (is (= (:dim sp) 2))
    (is (= (:bits sp) 3))
    (is (= (:radix sp) 7))
    (is (= (:digits sp) [0 1 2 3 4 5 6]))
    (is (= (:max-len sp) 17))
    (is (= (:pows sp)
           [1 7 49 343 2401 16807 117649 823543 5764801 40353607
            282475249 1977326743 13841287201 96889010407 678223072849
            4747561509943 33232930569601 232630513987207]))
    (is (= (:inv-lut sp) [0 6 5 4 3 2 1]))
    (is (= (:add-lut sp)
           [0 1 2 3 4 5 6
            1 2 3 4 5 6 0
            2 3 4 5 6 0 1
            3 4 5 6 0 1 2
            4 5 6 0 1 2 3
            5 6 0 1 2 3 4
            6 0 1 2 3 4 5]))
    (is (= (:add-carry-lut sp)
           [nil nil nil nil nil nil nil
            nil 1   nil 3   nil 1   nil
            nil nil 2   2   nil nil 6
            nil 3   2   3   nil nil nil
            nil nil nil nil 4   5   4
            nil 1   nil nil 5   5   nil
            nil nil 6   nil 4   nil 6]))
    (is (= (:mul-lut sp)
           [0 0 0 0 0 0 0
            0 1 2 3 4 5 6
            0 2 4 6 1 3 5
            0 3 6 2 5 1 4
            0 4 1 5 2 6 3
            0 5 3 1 6 4 2
            0 6 5 4 3 2 1]))))

(deftest address-creation
  (testing "Addresses in GBT2"
    (is (= (int->address sp 25) [4 3]))
    (is (= (address->string sp [4 3]) "34"))
    (is (= (address->int sp [4 3]) 25))
    (is (= (conform sp [12 11]) [4 3]))))

(deftest lookups
  (testing "GBT2 LUT usage"
    (is (= (lookup-add sp 6 6) 5))
    (is (= (lookup-carry sp 6 6 ) 6))))

(def aggregate1 (mapv (partial int->address sp) (:digits sp)))

(deftest operations
  (testing "Unary/binary operations on addresses in GBT2"
    (is (= (:inv-lut sp)
           (->> aggregate1
                (map (partial inv sp))
                (map (partial address->int sp)))))
    (is (= (add sp [1]) [1]))
    (is (= (reductions (partial add sp) origin (repeat 5 [1]))
           '([0] [1] [2 1] [3 1] [4 4 3] [5 4 3])))
    (is (= (sub sp [3 1]) [4 6]))
    (is (= (reductions (partial sub sp) [5 4 3] (repeat 5 [1]))
           '([5 4 3] [4 4 3] [3 1] [2 1] [1] [0])))
    (is (= (mul sp [1]) [1]))
    (is (= (map (partial mul sp [3 1]) (rest aggregate1))
           '([3 1] [6 2] [2 3] [5 4] [1 5] [4 6])))))
