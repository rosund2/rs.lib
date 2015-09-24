(ns rs.lib.poker-test
  (:require [clojure.test :refer :all]
            [rs.lib.poker :refer :all]))

(deftest hand-maker []
  (testing "creating pairs"
    (let [pairs (pp "AA")]
      (is (= 6 (count pairs)), "does not contain six pairs")
      (is (empty? (filter #(= (first %) (second %)) pairs))), "should not include same suit")

    (let [suited (sc "AK")]))


  (testing "creating offsuits"
    (let [offsuit (oc "AK")]
      (is (= 4 (count offsuit)) "should contain 4 sets"))))




