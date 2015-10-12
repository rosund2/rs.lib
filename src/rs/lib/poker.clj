(ns rs.lib.poker)

;;Read this:
;;http://www.blackrain79.com/2015/08/flop-strategies-versus-bad-poker.html
;;
;;M-( wrap round
;;M-r Raise
;;C-s C-w ++ search for current


(defn- map2v
  "applies f to all the values in a nested seq"
  [f v]
  (mapv #(mapv f %) v))

(def suits #{:club :diamond :spade :heart})
(def suit->str {:club "c" :diamond "d" :spade "s" :heart "h"})
(def ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(def all-card-ranks
  "a list of all hand combos"
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
  {:pre [(string? rank) (keyword suit)]}
  {:suit suit :rank rank})

(defn card-same-suit? [a b]
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
                   :when (not (card-same-suit? c d))]    
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
                   :when (card-same-suit? c d)]    
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


(defn deck-get-wcc
  "retrieves a wholecard combo from a given deck position"
  [{:keys [cards]} a b]
  (nth (nth cards a) b))

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


(defn deck-range-select-wcc
  "sets the specified wholecards in the wc combo collections to inrange? true"
  [deck a b]
  (update-in deck [:cards a b]
             (fn [wcc] (map2v #(assoc % :inrange? true) wcc))))


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
         (let [deck (deck-range-select-wcc deck a b)]
           (recur deck
                  (+ n (wcc-inrange-count
                        (deck-get-wcc deck a b)))
                  (rest path))))))))



(defn deck-wc-inrange-count
  "count the number of inrange combos in the deck"
  [deck]
  (reduce (fn [count wc]
            (if (seq (filter :inrange? wc))
              (inc count) count)
            )
          0
          ;; Creating a seq down to hand combo level
          (reduce (fn [total-combos range]
                    (reduce (fn [combos combo]
                              (reduce conj combos combo))
                            total-combos
                            range))
                  []
                  (:cards deck))))



;; Not done
;; Algorithm:
;; not decided
(defn deck-range-ppstring
  "make a pretty string of the selected range.
  notation should be inline with 
  http://www.pokerstrategy.com/strategy/others/2244/1/"

  [deck]

  (map2v (fn [x]           
           x
           ) deck)
  ;; walk-deck-by-path rankv
  #_(map (fn [x]        
         (let [wcc (deck-get-wcc deck (first x) (second x))]
           "AA"
           ))

       (deck))
  ;; make-wcc-pp-string 

  )
 
