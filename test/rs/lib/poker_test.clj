(ns rs.lib.poker-test
  (:require [clojure.test :refer :all]
            [rs.lib.poker :refer :all]))

(deftest hand-matching
  "tests related to matching poker hand types"
  []

  (let [cardm {
               :pair      [(make-card "A" :club) (make-card "K" :diamond) (make-card "K" :club) (make-card "8" :heart) (make-card "2" :club)]
               :twopair   [(make-card "A" :club) (make-card "K" :diamond) (make-card "K" :club) (make-card "8" :heart) (make-card "8" :club)]
               :royal     [(make-card "A" :club) (make-card "Q" :club) (make-card "K" :club) (make-card "J" :club) (make-card "T" :club)]
               :royal2    [(make-card "A" :club) (make-card "Q" :club) (make-card "Q" :diamond) (make-card "K" :club) (make-card "J" :club) (make-card "T" :club)]
               :fourkind  [(make-card "A" :club) (make-card "A" :club) (make-card "A" :club) (make-card "A" :club) (make-card "T" :club)]
               :fourkind2 [(make-card "A" :club) (make-card "Q" :club) (make-card "Q" :diamond) (make-card "K" :club) (make-card "J" :club) (make-card "T" :club)]
               :boat      [(make-card "A" :club) (make-card "A" :club) (make-card "A" :club) (make-card "Q" :club) (make-card "Q" :club)]
               :boat2     [(make-card "A" :club) (make-card "Q" :club) (make-card "Q" :diamond) (make-card "K" :club) (make-card "J" :club) (make-card "T" :club)]
               }
        ]
    
    (testing "matching one and two pairs"
      (is (= :pair (:type (pair-match (:pair cardm)))))
      (is (= :twopair (:type (pair-match (:twopair cardm))))))

    (testing "matching the royal"
      (is (= :royalstr8flush (:type (roystr8flush-match (:royal cardm)))))
      (is (= :royalstr8flush (:type (roystr8flush-match (:royal2 cardm))))))

    (testing "matching the str8flush"
      (is (= :str8flush (:type (str8flush-match (:royal cardm)))))
      (is (= :str8flush (:type (str8flush-match (:royal2 cardm))))))

    (testing "matching fourkind"
      (is (= :fourkind (:type (fourkind-match (:fourkind cardm)))))
      (is (= nil (:type (fourkind-match (:fourkind2 cardm))))))
    
    (testing "matching boat"
      (is (= :boat (:type (boat-match (:boat cardm)))))
      (is (= nil (:type (boat-match (:boat2 cardm))))))))


