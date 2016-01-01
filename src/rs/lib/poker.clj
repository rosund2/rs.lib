(ns rs.lib.poker)

;;Read this:
;;http://www.blackrain79.com/2015/08/flop-strategies-versus-bad-poker.html
;;
;;M-( wrap round
;;M-r Raise
;;C-s C-w ++ search for current



(defn- permutate-all
  "create all permutations of every a in v into a new vector b with [[a1 a2]..]"
  [v]
  (into []
        (for [a v b v :while (not= a b)]  [a b])))


(defn- map2v
  "applies f to all the values in a nested seq"
  [f & arg1]
  (apply mapv (fn [& arg2] (apply mapv f arg2))  arg1))


(defn in? [seq val]
  (some #(= val %) seq))

(def suits #{:club :diamond :spade :heart})
(def suit->str {:club "c" :diamond "d" :spade "s" :heart "h"})
(def ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])
(def rankmap {"A" 14 "K" 13 "Q" 12 "J" 11 "T" 10 "9" 9 "8" 8 "7" 7 "6" 6 "5" 5 "4" 4 "3" 3 "2" 2})

(def hand-combos
  "a list of all possible hand combos"
  (let [m (-> (reduce
               #(let [a (first (seq %2))
                      b (second (seq %2))]
                  (if (= a b)
                    (update-in %1 [:pp] (fnil conj []) %2)
                    (update-in %1 [:reg] (fnil conj []) %2)))
               {}
               (mapcat (fn [x r] (map #(str x %) (drop r ranks))) ranks (range)))
              (#(assoc %1 :sc (:reg %1))))]
    
    (->>
     (into [] (:pp m))
     (into (map #(str % "o") (:reg m)))
     (into (map #(str % "s") (:sc m))))))

(defn make-card [rank suit]
  {:pre [(string? rank) (keyword suit) (suits suit) (in? ranks rank)]}
  {:suit suit :rank rank})

(defn same-suit? [a b]
  (= (:suit a) (:suit b)))

(defn- make-card-variants
  "makes all the variants for a card (Ac, Ad, As, Ah) ie"
  [rank]
  {:pre [(string? rank)]}
  (into [] (for [suit suits
                 rankv [rank]]
             (make-card rankv suit))))

(defn pp
  "creates all combinations of a pocker pair"
  [hc]
  {:pre [(string? hc) (= 2 (count (seq hc)))]}
  (let [v (.toString (first hc))]
    (into [] (for [c (make-card-variants v)
                   d (make-card-variants v)
                   :while (not= c d)]    
               [c d]))))

(defn oc
  "creates all combinations of an unsuited combo"
  [hc]

  {:pre [(string? hc)
         (= 2 (count (seq hc)))
         (not= (first hc) (second hc))]}

  (let [c1 (.toString (first hc))
        c2 (.toString (second hc))]
    (into [] (for [c (make-card-variants c1)
                   d (make-card-variants c2)
                   :when (not (same-suit? c d))]    
               [c d]))))

(defn sc
    "creates all combinations of a suited combo"
  [hc]
  {:pre [(string? hc)
         (= 2 (count (seq hc)))
         (not= (first hc) (second hc))]}

  (let [c1 (.toString (first hc))
        c2 (.toString (second hc))]
    (into [] (for [c (make-card-variants c1)
                   d (make-card-variants c2)
                   :when (same-suit? c d)]    
               [c d]))))


(defmacro make-deck
  "creates a deck"
  [deck]
  {:cards deck
   :map (apply list '-> {}
               (mapcat
                (fn [row r1]
                  (map (fn [col r2]
                         `(assoc
                           ~(let [s (resolve (first col))
                                  ranks (second col) ]                                 
                              (cond
                                (= s #'pp)
                                ranks
                                (= s #'oc)
                                (str ranks "o")
                                (= s #'sc)
                                (str ranks "s")
                                :else
                                (-> (Exception. (str "unresolvable symbol: " (first col))) throw)))
                     [~r1 ~r2]))
                 row (range))     
            ) deck (range))) })
   

(defmacro defdeck
  "defs a deck"
  [name deck]  
  (list 'def name
     `(make-deck ~deck)))


(defdeck deck
  [[(pp "AA") (sc "AK") (sc "AQ") (sc "AJ") (sc "AT") (sc "A9") (sc "A8") (sc "A7") (sc "A6") (sc "A5") (sc "A4") (sc "A3") (sc "A2")]
   [(oc "AK") (pp "KK") (sc "KQ") (sc "KJ") (sc "KT") (sc "K9") (sc "K8") (sc "K7") (sc "K6") (sc "K5") (sc "K4") (sc "K3") (sc "K2")]
   [(oc "AQ") (oc "KQ") (pp "QQ") (sc "QJ") (sc "QT") (sc "Q9") (sc "Q8") (sc "Q7") (sc "Q6") (sc "Q5") (sc "Q4") (sc "Q3") (sc "Q2")]
   [(oc "AJ") (oc "KJ") (oc "QJ") (pp "JJ") (sc "JT") (sc "J9") (sc "J8") (sc "J7") (sc "J6") (sc "J5") (sc "J4") (sc "J3") (sc "J2")]
   [(oc "AT") (oc "KT") (oc "QT") (oc "JT") (pp "TT") (sc "T9") (sc "T8") (sc "T7") (sc "T6") (sc "T5") (sc "T4") (sc "T3") (sc "T2")]
   [(oc "A9") (oc "K9") (oc "Q9") (oc "J9") (oc "T9") (pp "99") (sc "98") (sc "97") (sc "96") (sc "95") (sc "94") (sc "93") (sc "92")]
   [(oc "A8") (oc "K8") (oc "Q8") (oc "J8") (oc "T8") (oc "98") (pp "88") (sc "87") (sc "86") (sc "85") (sc "84") (sc "83") (sc "82")]
   [(oc "A7") (oc "K7") (oc "Q7") (oc "J7") (oc "T7") (oc "97") (oc "87") (pp "77") (sc "76") (sc "75") (sc "74") (sc "73") (sc "72")]
   [(oc "A6") (oc "K6") (oc "Q6") (oc "J6") (oc "T6") (oc "96") (oc "86") (oc "76") (pp "66") (sc "65") (sc "64") (sc "63") (sc "62")]
   [(oc "A5") (oc "K5") (oc "Q5") (oc "J5") (oc "T5") (oc "95") (oc "85") (oc "75") (oc "65") (pp "55") (sc "54") (sc "53") (sc "52")]
   [(oc "A4") (oc "K4") (oc "Q4") (oc "J4") (oc "T4") (oc "94") (oc "84") (oc "74") (oc "64") (oc "54") (pp "44") (sc "43") (sc "42")]
   [(oc "A3") (oc "K3") (oc "Q3") (oc "J3") (oc "T3") (oc "93") (oc "83") (oc "73") (oc "63") (oc "53") (oc "43") (pp "33") (sc "32")]
   [(oc "A2") (oc "K2") (oc "Q2") (oc "J2") (oc "T2") (oc "92") (oc "82") (oc "72") (oc "62") (oc "52") (oc "42") (oc "32") (pp "22")]])


(def hand-ranks
  "ranking of hands from best to lowest
  taken from http://holdemtight.com/pgs/od/oddpgs/3-169holdemhands.htm" 
  [
   "AA" "KK" "QQ" "AKs" "JJ" "AQs" "KQs" "AJs" "KJs" "TT" "AKo" 
   "ATs" "QJs" "KTs" "QTs" "JTs" "99" "AQo" "A9s" "KQo" "88" 
   "K9s" "T9s" "A8s" "Q9s" "J9s" "AJo" "A5s" "77" "A7s" "KJo" 
   "A4s" "A3s" "A6s" "QJo" "66" "K8s" "T8s" "A2s" "98s" "J8s" 
   "ATo" "Q8s" "K7s" "KTo" "55" "JTo" "87s" "QTo" "44" "22" 
   "33" "K6s" "97s" "K5s" "76s" "T7s" "K4s" "K2s" "K3s" "Q7s" 
   "86s" "65s" "J7s" "54s" "Q6s" "75s" "96s" "Q5s" "64s" "Q4s" 
   "Q3s" "T9o" "T6s" "Q2s" "A9o" "53s" "85s" "J6s" "J9o" "K9o" 
   "J5s" "Q9o" "43s" "74s" "J4s" "J3s" "95s" "J2s" "63s" "A8o" 
   "52s" "T5s" "84s" "T4s" "T3s" "42s" "T2s" "98o" "T8o" "A5o" 
   "A7o" "73s" "A4o" "32s" "94s" "93s" "J8o" "A3o" "62s" "92s" 
   "K8o" "A6o" "87o" "Q8o" "83s" "A2o" "82s" "97o" "72s" "76o" 
   "K7o" "65o" "T7o" "K6o" "86o" "54o" "K5o" "J7o" "75o" "Q7o" 
   "K4o" "K3o" "96o" "K2o" "64o" "Q6o" "53o" "85o" "T6o" "Q5o" 
   "43o" "Q4o" "Q3o" "74o" "Q2o" "J6o" "63o" "J5o" "95o" "52o" 
   "J4o" "J3o" "42o" "J2o" "84o" "T5o" "T4o" "32o" "T3o" "73o" 
   "T2o" "62o" "94o" "93o" "92o" "83o" "82o" "72o"])

(defn rank-seq
  "Creates a highest to lowest coord map into a deck based on the deck map and a ranking vector"
  [deck rankv]
  {:pre [(map? deck) (map? (:map deck))]}
  (when (seq rankv)
    (cons ((:map deck) (first rankv))
          (lazy-seq
           (rank-seq deck (rest rankv))))))

(defn wcc-inrange-count
  "counts the number of wholecards which is inrange in a combo collection"
  [wcc]  
  {:pre [(vector? wcc)]}
  
  (count (filter (fn [wc] (some :inrange? wc)) wcc)))


(defn deck-get-wcc
  "retrieves a wholecard combo from a given deck position"
  [{:keys [cards]} a b]
  (nth (nth cards a) b))


(defn deck-range-select-wcc
  "sets the wholecards in the wc combo collections upto max to inrange? true "
  [deck a b max]

  (update-in deck [:cards a b]
             (fn [wcc]
               ;; TODO: Pretty sure i can write this as a simple map
               ;;       instead of this overly complex stuff
               (loop [wcc  wcc
                      akk (empty wcc)
                      count 0]
                 (if (empty? wcc)
                   akk
                   (recur
                    (rest wcc)
                    (conj akk
                          (if (>= count max)
                            (first wcc)
                            (mapv #(assoc % :inrange? true) (first wcc))))
                    (inc count)))))))


(defn deck-inrange-count
  "returns the total inrange count in the deck"
  [deck]
  (reduce (fn [count wc]
            (if (seq (filter :inrange? wc))
              (inc count) count))
          0
          ;; Reduce deck to a flat list of wholecards
          (reduce (fn [total-combos range]
                    (reduce (fn [combos combo]
                              (reduce conj combos combo))
                            total-combos
                            range))
                  []
                  (:cards deck))))


(defn deck-range-select
  "follow a path [[a b]] over the deck and selects wc up to the nlimit of combinations"

  ([deck nlimit]
   (deck-range-select deck (rank-seq deck hand-ranks) nlimit))
  
  ([deck path nlimit]  
   (loop [deck deck
          ;; number of combos we have selected
          n 0                
          path (seq path)]
     (let [[a b] (first path)]        
       (cond
         ;; if we are over the path limit
         ;; or we dont have a path anymore to
         ;; follow we just return deck
         (or (>= n nlimit)
             (nil? (first path)))
         deck
         
         :else
         ;; select all wc combos at the coord
         ;; note: we might select more combos than
         ;; asked for but lets just view it as an
         ;; approximation for now
         (let [deck (deck-range-select-wcc deck a b (- nlimit n))]
           (recur deck
                  (deck-inrange-count deck)
                  (rest path))))))))



(defn percentage-to-combo
  "converts from a percentage to a number of combos"
  [n]
  {:pre [(number? n)]}
  (* (/ (* 52 51) 2)       
     (/ n 100)))



;; Trying to define a system for defining ranges
;; AA-JJ ; 
;; AsAc (AcAd, AcAs, AcAh)
;; AKss (AsKs,AcKc,AhKh,AdKd) same suit
;; AKss-JTss, All same suit; AKs, AQs,AJs, KQs, KJs, QJs, QTs, JTs (should we limit on no gappers)
;; AcK-Ac2, all A clubs down to Ac2x
;; Ac4-Kc4, AcK-Ac4, KcQ-Kc4, 

(def handx
  [(make-card "A" :club) (make-card "K" :diamond) (make-card "K" :club) (make-card "8" :heart) (make-card "2" :club)])

(def handy
  [(make-card "A" :club) (make-card "K" :diamond) (make-card "K" :club) (make-card "8" :heart) (make-card "8" :club)])

(def handr
  [(make-card "A" :club) (make-card "Q" :club) (make-card "K" :club) (make-card "J" :club) (make-card "T" :club)])

(def handstr8
  [(make-card "2" :spade) (make-card "A" :club) (make-card "Q" :club) (make-card "J" :club) (make-card "T" :club) (make-card "9" :club) (make-card "8" :club)])



(defn- atleast-five-same-suit [hands]
  (second (first (seq (filter #(<=  5 (count (second %))) (group-by :suit hands))))))

(defn- larger-than [f]
  (fn [a b] (> (rankmap (f a)) (rankmap (f b)))))

(defn- sort-cards [hands]
  (sort (larger-than :rank) hands))

(defn- five-or-less-consecutive [cards]
  (reduce (fn [agg card]
            (if (== (count agg) 5)
              agg
              (let [rank (rankmap (:rank card))
                    latest (last agg)]
                (if (nil? latest)
                  (conj agg card)
                  (let [last-rank (rankmap (:rank latest))]
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
                   high-match]]

    (if-let [matcher (first matchers)]
      (let [match (matcher cards)]
        (if-not match
          (recur (rest matchers))
          match))

      ;; throw exception because this should not ever happen
      (.throw (Exception. "There should always be a a match:" cards))))

  




  

  )
