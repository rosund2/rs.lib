(ns rs.lib.poker
  (:require [clojure.math.combinatorics :as combo]))

(defrecord Card [rank suit])
(defrecord Hand [a b])
(defrecord Range [])

(def ranks [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king :ace])
(def suits #{:club :spade :heart :diamond})

(defn rank> [a b]
  (let [m (into {} (map-indexed (fn [e i] [i e]) ranks))]
    (> (m a) (m b))))

(defn make-card [r s]
  {:pre [(suits s) (some #(= r %) ranks)]}
  (->Card r s))

(defn make-hand [a b]
  {:pre [(= (type a) rs.lib.poker.Card)
         (= (type b) rs.lib.poker.Card)]}
  (->Hand a b))

(def deck (for [rank ranks suit suits] (make-card rank suit)))

;;
;; Above is considered core
;;

(defn- atleast-five-same-suit [hands]
  (second (first (seq (filter #(<=  5 (count (second %))) (group-by :suit hands))))))

(defn- larger-than [f]
  (fn [a b] (rank> (f a) (f b))))

(defn- sort-cards [hands]
  (sort (larger-than :rank) hands))

(defn- five-or-less-consecutive [cards]
  (reduce (fn [agg card]
            (if (== (count agg) 5)
              agg
              (let [rankindex (into {} (map-indexed (fn [e i] [i e]) ranks))
                    rank (rankindex (:rank card))
                    latest (last agg)]
                (if (nil? latest)
                  (conj agg card)
                  (let [last-rank (rankindex (:rank latest))]
                    (if (== (inc rank) last-rank)
                      (conj agg card)
                      (conj [] card))))))) [] (sort-cards cards)))


(defn pair-match
  "returns a hand-rank if one or twp pairs is found"
  [hand]

  (let [grouped-by-rank (group-by :rank hand)
        the-rest (into [] (mapcat val (filterv
                                       (fn [x] (not= (count (second x)) 2))
                                       grouped-by-rank)))
        only-pairs (filterv
                    (fn [x] (== (count (second x)) 2))
                    grouped-by-rank)]

    (cond
      (== (count only-pairs) 2)
      {:type :twopair :rank
       (mapv second
             (sort (larger-than first)
                   only-pairs)) :rest (vec (take 1 the-rest))}
      
      (== (count only-pairs) 1)
      {:type :pair :rank (val (first only-pairs)) :rest (vec (take 3 the-rest))}

      :else
      nil)))


(defn roystr8flush-match
  "returns a hand-rank of royal if match"
  [hand]
  (let [high-to-low (sort-cards (atleast-five-same-suit hand))]
    (when (= "AKQJT" (reduce str (map :rank high-to-low)))
      {:type :royalstr8flush :suit (:suit (first high-to-low))})))


(defn str8flush-match
  "name says it all"
  [cards]
  (let [high-to-low (sort-cards (atleast-five-same-suit cards))
        consecutive (five-or-less-consecutive high-to-low)]
    
    (if (= 5 (count consecutive))
      {:type :str8flush :rank consecutive})))

(defn fourkind-match
  "name says it all"
  [hands]
  (when-let [highest-four-kind (second (first (sort (larger-than first) #_(> (rankmap (first %1)) (rankmap (first %2)))
                                             ;; filter out different than four
                                             (filter #(= 4 (count (second %))) (group-by :rank hands)))))]
    {:type :fourkind
     :rank highest-four-kind
     ;; filtering our and sorting the hands that does not match up 4 kind
     :rest (sort-cards (vec (apply disj (set hands) highest-four-kind )))}))

(defn boat-match
  "name says it all"
  [cards]
  (let [by-rank (group-by :rank cards)
        pairs (second (first (sort (larger-than first)
                                   (filter #(= 2 (count (val %))) by-rank))))
        trips (second (first (sort (larger-than first)
                                   (filter #(= 3 (count (val %))) by-rank))))]
    (when (and (seq pairs) (seq trips))
          {:type :boat :rank [trips pairs]})))

(defn flush-match [cards]
  (when-let [high-to-low (seq (sort-cards (atleast-five-same-suit cards)))]
    {:type :flush :suit (:suit (first high-to-low)) :rank (vec high-to-low)}))

(defn str8-match [cards]
  (let [five-or-less (five-or-less-consecutive cards)]
    (when (= 5 (count five-or-less))
      {:type :str8 :rank five-or-less})))

(defn trips-match [cards]
  (let [by-rank (group-by :rank cards)
        trips (second (first (sort (larger-than first)
                                   (filter #(= 3 (count (val %))) by-rank))))]
    (when (seq trips)
      {:type :trips :rank trips
       :rest (sort-cards (vec (apply disj (set cards) trips )))})))

(defn highcard-match [cards]
  {:type :highcard :rank (vec (sort-cards cards))})

(defn hand-match
  "returns the best hand
  {:type :highcard|pair|twopair|trips|str8|flush|boat|4kind|str8flush|roystr8flush}"
  
  [cards]

  (loop [
         matchers [roystr8flush-match
                   str8flush-match
                   fourkind-match
                   boat-match
                   flush-match
                   str8-match
                   trips-match
                   pair-match
                   highcard-match]]

    (if-let [matcher (first matchers)]
      (let [match (matcher cards)]
        (if-not match
          (recur (rest matchers))
          match))

      ;; throw exception because this should not ever happen
      (.throw (Exception. "There should always be a a match:" cards)))))



;;;;;;

;; need a likelyhood to win based on a specific hand-combo
;; or a range (which should in theory be just a simple variant of the above)
;; Exp(wc + (combos flop)) vs Exp(wc1 + (combos flop))
;; Calculate all the hand types for hand one and two h11 vs h21++
;; (p11)



(def game
  {:positions {:button {:name "nisse"  :range [(make-card :ace :club) (make-card :king :club)]}
               
               :bb     {:name "troll"  :range [(make-card :five :club) (make-card :six :club)]}}

   :preflop {:action [{}]}

   :flop nil

   :turn nil

   :river nil})


(defn combine [v n]
  (combo/combinations v n))


(defn process-game [game]
  

  )










