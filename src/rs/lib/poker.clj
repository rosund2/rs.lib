(ns rs.lib.poker)


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


;; create range from percentage
(defn range-maker [n])

;; go from range to combos
(defn combos [s] )

;; what a range look like
(def hand-range ["A3s+" "A3o+" "Ac3o+" "AcKs"])


;;
;; How many combos of cards can we have
;; I want a program that i give a range lets say 10% and that it returns me a minimum formatted list of ranges
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def deck [["A" ::club]    ;;
;;            ["A" ::heart]   ;;
;;            ["A" ::diamond] ;;
;;            ["A" ::spade]]) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is a suitable datastructure for that ?
;;"AA" "AKS" "AQs"
;;"AK" "KK" "KQs"
;;"AQ" "KQ" "QQ"

(defn- make-suits [v]
  (into [] (for [x [:a :b :c :d]
                 y [v]]
             [y x])))

(defn pp [hc]
  "creates all combinations of hands for a pair"
  {:pre [(string? hc) (= 2 (count (seq hc)))]}
  (let [v (.toString (first hc))]
    (into [] (for [c (make-suits v)
                   d (make-suits v)
                   :while (not= c d)]    
               [c d]))))

(defn oc [hc])
(defn sc [hc])

(defn make-deck []
  [(pp "AA") (sc "AK") (sc "AQ")]
  [(oc "AK") (pp "KK") (sc "KQ")]
  [(oc "AQ") (oc "KQ") (pp "QQ")])














