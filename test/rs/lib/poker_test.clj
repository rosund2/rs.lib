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




