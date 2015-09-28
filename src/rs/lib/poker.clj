(ns rs.lib.poker)

;;Read this:
;;http://www.blackrain79.com/2015/08/flop-strategies-versus-bad-poker.html
;;
;;M-( wrap round
;;M-r Raise
;;C-s C-w ++ search for current

(defn how-often [n1 n2]
  "calculates how often you will need to win the pot by betting. n1 = bet, n2 = pot"
  (/ n1 (+ n1 n2)))

(def total-combos (/ (* 52 51) 2))

(defn probability [o a] (/ o a))

(defn equity-of-raise [openc foldc raisec pot raise]
  (+
     ;; Loose the pot
   (* (- raise) (probability raisec openc))
     ;; Win the pot
   (* (+ pot) (probability foldc openc))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what a range look like                          ;;
;; (def hand-range ["A3s+" "A3o+" "Ac3o+" "AcKs"]) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def cards [{"A" ::club}    ;;
;;            {"A" ::heart}   ;;
;;            {"A" ::diamond} ;;
;;            {"A" ::spade}]) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def suits #{:club :diamond :spade :heart})
(def suit->str {:club "c" :diamond "d" :spade "s" :heart "h"})

(defn make-card [rank suit]
  {:pre [(string? rank) (keyword suit)]}
  {:suit suit :rank rank})

(defn card-same-suit? [a b]
  (= (:suit a) (:suit b)))

(defn make-card-variants
  "makes all the variants for a card (Ac, Ad, As, Ah) ie"
  [v]
  {:pre [(string? v)]}
  (into [] (for [suit suits
                 rank [v]]
             (make-card rank suit))))

(defn pp
  "creates all combinations of a pocker pair"
  [hc & {:keys [value] :or {:value 169}}]
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

(def deck
  [[(pp "AA" :value 1) (sc "AK" :value 5) (sc "AQ" :value 7) (sc "AJ") (sc "AT") (sc "A9") (sc "A8") (sc "A7") (sc "A6") (sc "A5") (sc "A4") (sc "A3") (sc "A2")]
   [(oc "AK" :value 6) (pp "KK" :value 2) (sc "KQ") (sc "KJ") (sc "KT") (sc "K9") (sc "K8") (sc "K7") (sc "K6") (sc "K5") (sc "K4") (sc "K3") (sc "K2")]
   [(oc "AQ") (oc "KQ") (pp "QQ" :value 3) (sc "QJ") (sc "QT") (sc "Q9") (sc "Q8") (sc "Q7") (sc "Q6") (sc "Q5") (sc "Q4") (sc "Q3") (sc "Q2")]
   [(oc "AJ") (oc "KJ") (oc "QJ") (pp "JJ" :value 4) (sc "JT") (sc "J9") (sc "J8") (sc "J7") (sc "J6") (sc "J5") (sc "J4") (sc "J3") (sc "J2")]
   [(oc "AT") (oc "KT") (oc "QT") (oc "JT") (pp "TT") (sc "T9") (sc "T8") (sc "T7") (sc "T6") (sc "T5") (sc "T4") (sc "T3") (sc "T2")]
   [(oc "A9") (oc "K9") (oc "Q9") (oc "J9") (oc "T9") (pp "99") (sc "98") (sc "97") (sc "96") (sc "95") (sc "94") (sc "93") (sc "92")]
   [(oc "A8") (oc "K8") (oc "Q8") (oc "J8") (oc "T8") (oc "98") (pp "88") (sc "87") (sc "86") (sc "85") (sc "84") (sc "83") (sc "82")]
   [(oc "A7") (oc "K7") (oc "Q7") (oc "J7") (oc "T7") (oc "97") (oc "87") (pp "77") (sc "76") (sc "75") (sc "74") (sc "73") (sc "72")]
   [(oc "A6") (oc "K6") (oc "Q6") (oc "J6") (oc "T6") (oc "96") (oc "86") (oc "76") (pp "66") (sc "65") (sc "64") (sc "63") (sc "62")]
   [(oc "A5") (oc "K5") (oc "Q5") (oc "J5") (oc "T5") (oc "95") (oc "85") (oc "75") (oc "65") (pp "55") (sc "54") (sc "53") (sc "52")]
   [(oc "A4") (oc "K4") (oc "Q4") (oc "J4") (oc "T4") (oc "94") (oc "84") (oc "74") (oc "64") (oc "54") (pp "44") (sc "43") (sc "42")]
   [(oc "A3") (oc "K3") (oc "Q3") (oc "J3") (oc "T3") (oc "93") (oc "83") (oc "73") (oc "63") (oc "53") (oc "43") (pp "33") (sc "32")]
   [(oc "A2") (oc "K2") (oc "Q2") (oc "J2") (oc "T2") (oc "92") (oc "82") (oc "72") (oc "62") (oc "52") (oc "42") (oc "32") (pp "22")]])

(defn deck-get-wcc [deck a b]
  (nth (nth deck a) b))

(defn wcc-inrange-count [wcc]
  (count (filter (fn [wc] (some :inrange? wc)) wcc)))

(defn wcc-matcher [f wcc] (some f wcc))

(def wcc-matcher-inrange? (partial wcc-matcher :inrange?))

(defn wcc-map-card [f wcc]
  (mapv #(mapv f %) wcc))

(defn deck-update-wcc [deck a b f]
  (update-in deck [a b] f ))

(defn deck-flatten-to-wc
  "Flattes the deck to a list of wholecards"
  [deck]
  (reduce (fn [total-combos range]
            (reduce (fn [combos combo]
                      (reduce conj combos combo))
                    total-combos range))
          [] deck))

(defn- deck-range-select-wcc
  "sets the specified wholecards in the wc combo collections to inrange? true"
  [deck a b]
  (deck-update-wcc deck a b
                   (fn [wcc] (wcc-map-card #(assoc % :inrange? true) wcc))))

(defn- deck-range-select-by-path
  "follow a path [[a b]] over the deck and selects wc up to the nlimit of combinations"
  [deck path nlimit]

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
                   (rest path)))))))



(defn deck-total-inrange-count
  "count the number of selected combos in the deck"
  [deck]
  (reduce (fn [count wc]
            (if (seq (filter :inrange? wc))
              (inc count) count)
            ) 0 (deck-flatten-to-wc deck)))



(defn deck-range-select
  "selects a number of wholecards combos in a deck based on best to worse"
  [deck nc]

  (let
      ;; Top pairs: AA,KK,QQ,JJ
      [top-pair-coords [[0 0] [1 1] [2 2] [3 3] [4 4]]] 

      (loop [methods [#(deck-range-select-by-path %1 top-pair-coords %2)]
          deck deck
          count 0]
     (if (or (empty? methods)
             (>= count nc))
       deck
       (let [deck ((first methods) deck (- nc count))]
         (recur
          (rest methods)
          deck
          (deck-total-inrange-count deck)))))))


(defn deck-range-ppstring
  "make a pretty string of the selected range.
  notation should be inline with 
  http://www.pokerstrategy.com/strategy/others/2244/1/"

  [deck]

  (->> (mapv #(deck-get-wcc deck % %) ;create a vector with a path of wcc's
             [0 1 2 3 4 5 6 7 8 9 10 11 12])

       ;;filter out wc that are not inrange?
       (mapv #(filterv wcc-matcher-inrange? %))  

       ;; filter out empty vectors
       (filterv seq) 

       ;; flatten to wholecard level
       (reduce (fn [akk wc] (apply conj akk wc)) [])

       ;; reduce to a vector of wholecards (no suit)
       (reduce (fn [akk [a b]]
                 (let [pp (str (:rank a) (:rank b))]
                   (if-not (some #(= % pp) akk)
                     (conj akk pp) akk)              
                   )
                 ) [])))
 
;; use case:
;; 1. set hand range to 10 or 144 combos
(let [x {":TPP" (* 6 4) ;;AA-JJ
         ":AJ+" (* 16 3);;AK,AQ,AJ
         ":MPP" (* 6 4) ;;TT-77
         ":KQ-JT" (* 3 16)} ;;JT,QJ,KQ
      ]
  (println x)
  (println (reduce + 0 (vals x)))
  )

(def range->valuemap
  [
   ;; Top pairs from JJ++
   {:TPP [[0 0] [1 1] [2 2] [3 3] [4 4]]}
   ;; AKs-KJs
   {:LSBR [[]]}
   ])
