(ns rs.lib.poker-test
  (:require [clojure.test :refer :all]
            [rs.lib.poker :refer :all]))

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
  (testing "selecting top pairs"

    (let [deck (deck-range-select (make-deck) 6)]
      (is (= 6 (wcc-inrange-count
                (deck-get-wcc deck 0 0))) "AA should be selected")

      (is (= 0 (wcc-inrange-count
                (deck-get-wcc deck 1 1))) "KK should not be selected"))
    
    (let [deck (deck-range-select (make-deck) 12)]
      (is (= 6 (wcc-inrange-count
                (deck-get-wcc deck 0 0))) "AA should be selected")
      
      (is (= 6 (wcc-inrange-count
                (deck-get-wcc deck 1 1))) "KK should be selected")
      
      (is (= 0 (wcc-inrange-count
                (deck-get-wcc deck 2 2))) "QQ should not be selected"))))




