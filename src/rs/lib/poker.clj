(ns rs.lib.poker)

;;Read this:
;;http://www.blackrain79.com/2015/08/flop-strategies-versus-bad-poker.html
;;
;;M-( wrap round
;;M-r Raise 

(defn how-often [n1 n2]
  "calculates how often you will need to win the pot by betting. n1 = bet, n2 = pot"
  (/ n1 (+ n1 n2)))

(def total-combos (/ (* 52 51) 2))

#_(defn how-many-combos [v] "[AKs]")

;; Question: if opponent raises 80% of hands, what does that range look like ?
;; If opponent raises 80% of hands and calls raise with 20% of hands


;; MP opens
;; 22+,A8s+,KTs+,QJs,T9s,98s, AJo+,KJo+ = 181 combinations

;; If we 3bet:
;; 4bets: QQ+,AKs = 22 combinations. We fold.
;; calls: 77-JJ,AQ+,98s,T9s = 61 combinations
;; folds: everything else = 98 combinations
;; EV(MP folds/4bets)=
;;                     22/181*(-6)
;;                   + 98/181*2
;;                   = -0.72+1.08=0.36

;; On the flop:
;; We cbet, he
;; calls: JJ,TT(will raise turn),T9 = 16 (our equity 9%, assume 0%)
;; raises: AKo = 12 
;; folds: AQ,98s,77-99 = 33
;; EV(calls preflop; call+raise+fold)=(16+12)/181*(-12) + 33/181*6 = -1.81+1.09 = -0.79

;; Total EV=-0.43

;; So 3betting against this range doesn't seem to be profitable. But if we were up against a loose button raiser who also raises 56s-78s, A2s-A7s, A8o-ATo+,98o and other stuff we can add at least 100 handcombinations to his range that he will have to fold preflop. 

;; In that case the total EV is
;; 22/281*(-2)+28/281*(-12)+33/281*(6)+198/281*2=
;; -0.15-1.2+0.7+1.4 = 0.75.

;; So when the openraiser has a tight range, 3betting is not good. If he's looser 3betting is +EV. On top of that calling is worse in the second case, because a lot of his range won't stack off if we hit a set. 

(defn probability [o a] (/ o a))

(defn equity-of-raise [openc foldc raisec pot raise]
  (+
     ;; Loose the pot
   (* (- raise) (probability raisec openc))
     ;; Win the pot
   (* (+ pot) (probability foldc openc))))


(equity-of-raise
 ;; opening combos
 200
 ;; folding combos
 180
 ;; raising combos
 50
 ;; pot size
 0.75
 ;; bet size
 0.9)


(defn combo-parser [s]
  {:pre [(string? s)]}
  (let [s (seq s)]
    (cond
    ;; PP
    (and (= 2 (count s))
         (= (first s) (last s)))
    (* 3 2)
    ;; AKo 
    (and
     (= 3 (count s))
     (not (= (first s) (second s)))
     (= \o (last s)))
    (* 4 3)
    ;; AKs
    (and
     (= 3 (count s))
     (not (= (first s) (second s)))
     (= \s (last s)))
    (* 4 1)
    :else
    ;; AK
    (* 4 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what a range look like                          ;;
;; (def hand-range ["A3s+" "A3o+" "Ac3o+" "AcKs"]) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def hand [["A" ::club]    ;;
;;            ["A" ::heart]   ;;
;;            ["A" ::diamond] ;;
;;            ["A" ::spade]]) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def suits #{:club :diamond :spade :heart})

(defn make-card [rank suit]
  {:pre [(string? rank) (keyword suit)]}
  {:suit suit :rank rank})

(defn make-card-suits [v]
  "makes all the variants for a card (Ac, Ad, As, Ah) ie"
  {:pre [(string? v)]}
  (into [] (for [suit suits
                 rank [v]]
             (make-card rank suit))))

(defn pp [hc]
  "creates all combinations of a pocker pair"
  {:pre [(string? hc) (= 2 (count (seq hc)))]}
  (let [v (.toString (first hc))]
    (into [] (for [c (make-card-suits v)
                   d (make-card-suits v)
                   :while (not= c d)]    
               [c d]))))

(defn oc [hc]
  "creates all combinations of an unsuited combo"
  {:pre [(string? hc)
         (= 2 (count (seq hc)))
         (not= (first hc) (second hc))]}

  (let [c1 (.toString (first hc))
        c2 (.toString (second hc))]
    (into [] (for [c (make-card-suits c1)
                   d (make-card-suits c2)
                   :when (not= (second c) (second d))]    
               [c d]))))

(defn sc [hc]
    "creates all combinations of a suited combo"
  {:pre [(string? hc)
         (= 2 (count (seq hc)))
         (not= (first hc) (second hc))]}

  (let [c1 (.toString (first hc))
        c2 (.toString (second hc))]
    (into [] (for [c (make-card-suits c1)
                   d (make-card-suits c2)
                   :when (= (second c) (second d))]    
               [c d]))))

(defn make-deck []
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

;; a specific AK KK 97 is called wholecards wc
;; combinations of their suit are called wc combos

(defn deck-read-wc-combos [deck a b]
  (nth (nth deck a) b))

(defn deck-update-wc-combos [deck a b f]
     (update-in deck [a b] f ))

(defn deck-flatten-to-wc [deck]
  (reduce (fn [total-combos range]
            (reduce (fn [combos combo]
                      (reduce conj combos combo))
                    total-combos range))
          [] deck)) 

(defn deck-range-select-tpp [deck nlimit]
  "selects TP wc combos in the deck up until we reach the selected number of combos"
  (let [range (seq #{[1 1] [2 2] [3 3] [4 4]})] ; AA/KK/QQ/JJ
    (loop [deck deck
           n 0                          ; number of combos we have selected
           range range]

      (let [[a b] (first range)]
        (cond
          ;; if we are over the range limit
          ;; or we dont have a path anymore to
          ;; follow we just return deck
          (or (>= n nlimit)
              (nil? (first range)))
          deck
          
          :else
          ;; 
          (let [deck (deck-update-wc-combos deck a b
                                            (fn [wcc] (map
                                                       (fn [x]
                                                         (do (println "asdasd" x) x))
                                                       ;;#(assoc % ::selected? true)
                                                       wcc)))]
           (recur deck
                  (+ n 1) ;; Should have an updated combo count
                  (rest range))))))))

(defn deck-range-select [deck d]
  "create a range from a percentage"
  ;; What path do we follow
  ;; Stop when selected combos / total > d
  )

(defn deck-range-ppstring [deck]
  "make a pretty string of the range selected"
  )





















