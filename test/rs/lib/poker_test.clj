(ns rs.lib.poker-test
  (:require [clojure.test :refer :all]
            [rs.lib.poker :refer :all]))

(deftest hand-matching
  "tests related to matching poker hand types"
  []

  (let [cardm {
               :pair      [(make-card :ace :club) (make-card :king :diamond) (make-card :king :club) (make-card :eight :heart) (make-card :two :club)]
               :twopair   [(make-card :ace :club) (make-card :king :diamond) (make-card :king :club) (make-card :eight :heart) (make-card :eight :club)]
               :royal     [(make-card :ace :club) (make-card :queen :club) (make-card :king :club) (make-card :jack :club) (make-card :ten :club)]
               :royal2    [(make-card :ace :club) (make-card :queen :club) (make-card :queen :diamond) (make-card :king :club) (make-card :jack :club) (make-card :ten :club)]
               :fourkind  [(make-card :ace :club) (make-card :ace :club) (make-card :ace :club) (make-card :ace :club) (make-card :ten :club)]
               :fourkind2 [(make-card :ace :club) (make-card :queen :club) (make-card :queen :diamond) (make-card :king :club) (make-card :jack :club) (make-card :ten :club)]
               :boat      [(make-card :ace :club) (make-card :ace :club) (make-card :ace :club) (make-card :queen :club) (make-card :queen :club)]
               :boat2     [(make-card :ace :club) (make-card :queen :club) (make-card :queen :diamond) (make-card :king :club) (make-card :jack :club) (make-card :ten :club)]
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


