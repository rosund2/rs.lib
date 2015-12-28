(ns rs.lib.poker-test
  (:require [clojure.test :refer :all]
            [rs.lib.poker :refer :all]))


(deftest deck-api []
  (testing "getting wholecard combos"
    (is (= 6 (count (deck-get-wcc deck 0 0))) "accessing aces, should return 6 combos")))

(deftest hand-maker []

  (testing "creating pairs"
    (let [pairs (pp "AA")]
      (is (= 6 (count pairs)), "should contain 6 wc")
      (is (empty? (filter #(= (first %) (second %)) pairs))), "should not include same suit"))

  (testing "creating offsuits"
    (let [offsuit (oc "AK")]
      (is (= 12 (count offsuit)) "should contain 12 wc")))
  
  (testing "creating suited"
    (let [offsuit (sc "AK")]
      (is (= 4 (count offsuit)) "should contain 4 wc"))))


(deftest wholecards-combos-select []
  (testing "selecting top pairs by combos"

    (let [deck (deck-range-select deck 6)]
      (is (= 6 (wcc-inrange-count
                (deck-get-wcc deck 0 0))) "AA should be selected")

      (is (= 0 (wcc-inrange-count
                (deck-get-wcc deck 1 1))) "KK should not be selected"))
    
    (let [deck (deck-range-select deck 12)]
      (is (= 6 (wcc-inrange-count
                (deck-get-wcc deck 0 0))) "AA should be selected")
      
      (is (= 6 (wcc-inrange-count
                (deck-get-wcc deck 1 1))) "KK should be selected")
      
      (is (= 0 (wcc-inrange-count
                (deck-get-wcc deck 2 2))) "QQ should not be selected")))


  (testing "selecting half a combo collection"
    (is (= 3  (wcc-inrange-count (deck-get-wcc (deck-range-select-wcc deck 0 0 3) 0 0)))))

  (testing "range selection by 10% = 136 combos"
    (let [deck (deck-range-select deck 10)]
      ;; 88++, AJ+, KJ+ (136 combos)
      (let [x {:TPP (* 6 7) :AJ+ (* 16 3) :JT+ (* 3 16)}]
        (println x)
        (println (reduce + 0 (vals x)))
        )))

  (testing "range count calculations"
    ;; deck-total-inrange-count
    (let [deck (deck-range-select deck 10)]
      (is (= 10 (deck-inrange-count deck)) "ten wc should be selected"))))


#_(deftest pprint-hand-range []
    (is (= "AA" (deck-range-ppstring (deck-range-select deck 6))))
    (is (= "KK+" (deck-range-ppstring (deck-range-select deck 12))))
    (is (= "44-66" (deck-range-ppstring (deck-range-select deck 6)))))


(deftest value-ranges []

  (testing "testing consistency of default value range"
    (is (= (count hand-ranks) 169) "a value map should contain values for all hands")
    (is (= (count hand-ranks) (count (into #{} hand-ranks))) "contains duplicate value")

    (is (empty? (clojure.set/difference (apply hash-set hand-combos) (apply hash-set hand-combos))) "value map should contain all hands")
    (is (empty? (clojure.set/difference (apply hash-set hand-combos) (apply hash-set hand-combos))) "value map should not contain hand values that are not allowed"))

  (testing "testing ranked seq generator"
    (is (= [0 0] (first (rank-seq deck hand-ranks))) "AA should be top value uno one")))


