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

  (testing "range selection by 10% = 136 combos"
    (let [deck (deck-range-select deck 10)]
      ;; 88++, AJ+, KJ+ (136 combos)
      (let [x {:TPP (* 6 7) :AJ+ (* 16 3) :JT+ (* 3 16)}]
        (println x)
        (println (reduce + 0 (vals x)))
        )

      )))


#_(deftest pprint-hand-range []
  (is (= "AA" (deck-range-ppstring (deck-range-select deck 6))))
  (is (= "KK+" (deck-range-ppstring (deck-range-select deck 12))))
  (is (= "44-66" (deck-range-ppstring (deck-range-select deck 6)))))



(deftest value-ranges []
  (testing "testing consistency of default value range"
    (is (= (count valuemap) 169) "a value map should contain values for all hands")
    (is (= (count valuemap) (count (into #{} valuemap))) "contains duplicate value")

    ;; 
    (is (empty? (filter (fn [hand] (filter #(not= hand %1) valuemap)) (let [ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"]]
                                                                        


                                                                        ))))
    ))
