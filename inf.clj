
;scratch pad for moving inner loop fns over for release

;we're just gonna start throwing in infinite case fns...





(defmacro defn-memo [name & body] `(def ~name (memoize (fn ~body))))

;dotimes w/ starting point
(defmacro dorange
  [bindings & body]
  (let [i (first bindings)
        a (second bindings)
        b (last bindings)]
    `(let [n# (int ~b)]
       (loop [~i (int ~a)]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(def dealcount (atom 0))
(def discount (atom 0))
(def nosort (atom 0))
(def pCount (atom 0))
(def hitcount (atom #{}))

(def strat (atom {})) ;strategy for simulation
(def flags {:d-other (bit-shift-left 1 60) :d-ace (bit-shift-left 1 61) :d-ten (bit-shift-left 1 62)})

(def deck1 [2 3 4 5 6 7 8 9 10 10 10 10 1]) ;infinite deck
(def single-deck (vec (sort < (flatten (repeat 4 deck1)))))

(def short-freq-arr [[1 0 0 0 0 0 0] [0 1 0 0 0 0 0] [0 0 1 0 0 0 0] [0 0 0 1 0 0 0] [0 0 0 0 1 0 0] [0 0 0 0 0 1 0] [0 0 0 0 0 0 1]]) ;[0 0 0 0 0 0 1]]) ;??


(defn average-over-vec [the-vec] (/ (reduce + the-vec) (count the-vec)))
;consider recursive AND memo'd version (time to combine arrays might be high; and it's the inner loop)
(defn-memo average-elements [vec-list]
  (let [v-len (count vec-list)]
    (vec (for [i (vec (range (count (vec-list 0))))]
           (/ (reduce + (vec (for [v vec-list] (v i)))) v-len)))))


;specialty average-elements; **WORKS (I think)**
(defn-memo average-elements-noBJ [vec-list] ;memo here = 30% speed increase... everything still working otherwise??
  (let [v-len (count (filter #(not= (last short-freq-arr) %) vec-list))]  ;count here and ignore BJ results (to test at least)?? maybe prune at pre- deal-endpoint-inf entrypoint ??
    (vec (for [i (vec (range (count (vec-list 0))))]  ;**WAIT, don't we need to actually filter vec-list for this stage too?? [vec-list1 (filter... veclist) v-len (count vec-list1)] ??
           (/ (reduce + (vec (for [v vec-list] (v i)))) v-len)))))

(defn ave-elements [vecs] (/ (map (partial reduce +) (map vector vecs))) 6) ;should this work?
;OR do it iteratively, but with using a memo'd average-elements for each pair!! 
;  simplify to assume pair

;-----|-----
;**we're digging with position, not the value?? because rmCard uses position??
(defn doseqfor [combineby deck digwith]
  (do	(doseq [val deck]  (digwith val))
      (let [vallist (vec (for [val deck] (digwith val)))]
        (combineby vallist))))

;accepts hand, creates deck from hand+global starter (freq)deck
(def freq-deck (frequencies single-deck))


(def combos (for [i deck1 j deck1] (vec (sort < [i j]))))

(defn-memo rmCard [arr pos] (vec (concat (subvec arr 0 pos) (subvec arr (+ pos 1)))))  ;used outside of being hit (dealer or player)
(defn-memo addCard [arr card] (vec (sort < (conj arr card))))

(defn-memo BJHandVal [hand] 
  (let [tot (reduce + hand)] 
    (if (and (< tot 12) (= (hand 0) 1)) (+ tot 10) tot)))

(defn-memo dealerhands [[pHand dHand deck splits notes bet :as gm]]
	(let 	[up (dHand 0)
        dHands (for [i deck] (vec (sort < [up i])))
        hCombos (count dHands)]
   (filter #(not= % [1 10]) dHands)))

(def handset (atom #{}))
;Whoa, fast; how do we make it faster?
;  sort-memo? cumulative arrays? 
;  currently using s-f-a set for cummulative arrays
(defn-memo dealer-endpoint-freqs-inf [[dHand dTot outcards notes :as gm]]
  ;(print "dealer-endpoint-freqs-inf")
  (swap! dealcount inc)
  (swap! handset conj dHand)
  ;  (swap! handset conj dHand)
  (if (> dTot 16)
    (cond (> dTot 21) (first short-freq-arr) ;[1 0 0 0 0 0 0]
          (and (not (notes :d-other)) (notes :d-ace) (notes :d-ten)) (last short-freq-arr) ;[0 0 0 0 0 0 1]
          :else (short-freq-arr (- dTot 16)))
    (doseqfor average-elements-noBJ
              deck1
              #(let [dHand1 (vec (sort < (conj dHand %)))  ; (addCard) instead of (conj) ??
                     dTot1 (BJHandVal dHand1)]
                 (dealer-endpoint-freqs-inf
                   [dHand1
                    dTot1
                    (vec (sort < (conj outcards %)))
                    (conj notes (cond (= % 1) :d-ace (= % 10) :d-ten :else :d-other))])))))

(defn remove-BJs [v]
     (let [v1 (pop v)  ;where did we leave the BJs? on the end?  -> use before pop/float
           d (reduce + v1)]
       (map #(/ % d) v1)
       ))

;for reference:
;(defn average-elements-n [vec-list n]
 ;   (vec (for [i (vec (range (count (vec-list 0))))]  
  ;    (/ (reduce + (vec (for [v vec-list] (v i)))) n))))

;wait... we're assuming deck1? not presently passing outcards
(defn re-ave [v] (let [d (reduce + v)] (map #(/ % d) v)) ) 
(defn allups0 [] (vec (for [i (range 11)] (vec (map float (dealer-endpoint-freqs-inf [[i] i [] (cond (= i 10) #{:d-ten} (= i 1) #{:d-ace} :else #{:d-other})]))))))
(defn allups [] (vec (for [i deck1] (vec (map float (re-ave (pop (dealer-endpoint-freqs-inf [[i] i [] (cond (= i 10) #{:d-ten} (= i 1) #{:d-ace} :else #{:d-other})]))))))))
;(def ups (allups))
;(def ups (mapv (partial mapv float) (mapv remove-BJs (for [i [2 3 4 5 6 7 8 9 10 1]] (vec (dealer-endpoint-freqs-inf [[i] i [] (cond (= i 10) #{:d-ten} (= i 1) #{:d-ace} :else #{:d-other})]))))))   ;for (diffs), NOT (lut) -> use deck1
(def ups (mapv (partial mapv float) (mapv remove-BJs (for [i [1 2 3 4 5 6 7 8 9 10 10 10 10]] (vec (dealer-endpoint-freqs-inf [[i] i [] (cond (= i 10) #{:d-ten} (= i 1) #{:d-ace} :else #{:d-other})]))))))

;(defn-memo cummulative-postpeek-inf [gm] (for [i deck1] (pop (dealer-endpoint-freqs-inf [[i] i [] (cond (= i 10) #{:d-ten} (= i 1) #{:d-ace} :else #{:d-other})])))) ;gm = ?? outcards??

(def bjdeck [2 3 4 5 6 7 8 9 10 10 10 10 11]) ;(range 2 12)) ;inf… maybe use 10x4
(defn-memo dealer-endpoint-freqs-inf1 [[dTot outcards notes :as gm]]
  (swap! dealcount inc)
  (if (> dTot 16)
    (cond (> dTot 21) (first short-freq-arr) ;[1 0 0 0 0 0 0]
          (and (not (notes :d-other)) (notes :d-ace) (notes :d-ten)) (last short-freq-arr) ;[0 0 0 0 0 0 1]
          :else (short-freq-arr (- dTot 16)))
    (doseqfor average-elements-noBJ  ;**noBJ??
              ;single-deck 
              bjdeck
              #(let [[dTot1 notes1]
                     (cond (and (< 21 (+ % dTot)) (== % 11)) [(+ dTot 1) notes]
                           (and (< 21 (+ % dTot)) (notes :d-ace) (== % 10)) [(+ dTot % -10) (conj (disj notes :d-ace) :d-ten)]  ;might get more compression by clearing d-ten after 1st card??  1/10/11 ??
                           (and (< 21 (+ % dTot)) (notes :d-ace)) [(+ dTot % -10) (disj notes :d-ace)]
                           (== % 11) [(+ dTot 11) (conj notes :d-ace)]
                           (== % 10) [(+ dTot 10) (conj notes :d-ten)]
                           :else [(+ dTot %) (conj notes :d-other)])]
                 (dealer-endpoint-freqs-inf1 [dTot1 (vec (sort (conj outcards %))) notes1])))))

;(defn allups1 [] (vec (for [i bjdeck] (pop (vec (map float (dealer-endpoint-freqs-inf1 [i [] (cond (= i 10) #{:d-ten} (= i 11) #{:d-ace} :else #{:d-other})])))))))
(defn allups1 [] (vec (for [i bjdeck] (vec (map float (dealer-endpoint-freqs-inf1 [i [] (cond (= i 10) #{:d-ten} (= i 11) #{:d-ace} :else #{:d-other})]))))))

;(time (doseq [i bjdeck j bjdeck k bjdeck] (dealer-endpoint-freqs-inf1 [i (sort [i j k]) (if (== i 11) #{:d-ace} #{})])))
;(pmap #(time (doseq [i bjdeck j bjdeck k bjdeck] (dealer-endpoint-freqs-inf1 [i (sort (vec (concat (repeat % i) [j k]))) (if (== i 11) #{:d-ace} #{})]))) [1 2 3 4]) ;**more memory intense
;(time (doseq [i bjdeck j bjdeck k bjdeck l [1 2 3 4]] (dealer-endpoint-freqs-inf1 [i (sort (vec (concat (repeat l i) [j k]))) (if (== i 11) #{:d-ace} #{})])))
;(time (doseq [i bjdeck j bjdeck k bjdeck l [1 2 3 4]] (dealer-endpoint-freqs-inf1 [i (sort (vec (concat (repeat l i) [j k]))) (cond (= i 10) #{:d-ten} (= i 11) #{:d-ace} :else #{:d-other})])))





(defn cdw1 [dvec]
  (if (= (count dvec) 1) (* 0.5 (dvec 0)) ;can we just halve this??
      [(+ (* 0.5 (first dvec)) (reduce + (vec (rest dvec)))) (cdw1 (vec (rest dvec)))]))
(defn cum-dealer-win [dvec] (vec (flatten [(reduce + (rest dvec)) (cdw1 (vec (rest dvec)))])))
;*get dvec from (allups) ??

(defn convertAs [hand] (vec (for [i hand] (if (== i 1) 11 i))))  ;**SORT?
;(time (for [i bjdeck] (dealer-endpoint-freqs-inf1 [i (sort [5 6]) (cond (= i 10) #{:d-ten} (= i 11) #{:d-ace} :else #{:d-other})])))
(defn d-cum-win [[pHand dHand deck splits notes bet :as gm]]   ;**why -memo??
  ;(print "IN")
  (let [dHand1 (dHand 0) ;(convertAs dHand)
        ;_ (print "1")
        ;pHand1 (sort (convertAs pHand))
        ;_ (print "2")
        p-tot (BJHandVal pHand)  ;*from before changing to -freqs-inf1
        ;_ (print "3")
 ;       dUp (if (= (dHand 0) 1) 12 (- (dHand 0) 2))
        d-vec ;(cum-dealer-win (pop   ;**(dec dTot)**  can we just dec dUp? no... could be an A

        ;**what's stopping me from going straight in, and not realizing all 11 entries??  **must access using dHand1 NOT (dec dHand1), (for everything?) , then? dec for access below?
                                ;((mapv (partial mapv float) (mapv remove-BJs (for [i [1 2 3 4 5 6 7 8 9 10 10 10 10]] (vec (dealer-endpoint-freqs-inf [[i] i [] (cond (= i 10) #{:d-ten} (= i 1) #{:d-ace} :else #{:d-other})]))))) dHand1)  ;**why map float? = out slight split errs?

        (vec (remove-BJs (vec (dealer-endpoint-freqs-inf [[dHand1] ;dHand1
                                                          dHand1  ;-inf1
                                                          []
                                                         ;(sort (vec (concat dHand1 pHand1 (flatten (repeat splits (filter integer? notes)))))) ;**??**
                                                          (cond (= dHand1 10) #{:d-ten} (= dHand1 1) #{:d-ace} :else #{:d-other})]))))
                                ;))
        ]
    ;(print "POST")

    ;**add A for A in notes!!
                ;((allups) dUp))]  ;(dHand 0) is wrong... A = 1? 11? right, that's why T's work
     ;(print pHand dUp ((allups) dUp) cDeal)
    ;(if (< pHand2 17) (cDeal 0)  
    ;  (cDeal (- pHand2 16)))

    (cond (> p-tot 21) 0
          (> p-tot 16) (+ (* 0.5 (d-vec (- p-tot 16)))
                          (reduce + (subvec d-vec 0 (- p-tot 16))))  ;subvec?
          :else (first d-vec)))) ;cum-dealer-win??
    ;**where are we popping?? in (allups)

;**(- (- 2 (* 2 (d-cum-win gm))) 1)     ;**ok, so this isn't (allups) = no full chart on return**  we have the dealer's row, just point to player entry   **effecient**? should still get 10.5k deals


;why don't we have a separate fn that uses the dUp instead of dHand?? just to handle A's easily? should feed in [dTot dUp #{:d-ace}] (at least for inf decks)
(defn-memo deal-e5 [[pTot dHand deckS notes :as gm]] ;add pHand, subtract deckS; for non-infinite decks
  (let [dTot (BJHandVal dHand)] 
	(cond (> dTot 21) 1
       (or (< dTot 17) (and (< dTot 18) (not= dTot (reduce + dHand)) (notes :hitsoft)))   
            (doseqfor average-over-vec deck1 #(let [newdeck (rmCard deck1 %) newdHand (addCard dHand (deck1 %))] (deal-e5 [pTot newdHand deck1 notes])))
		   (= dTot pTot) 0.5
		   (< dTot pTot) (if (notes :bj) 1.5 1)
		   (> dTot pTot) 0)))

(defn deal [[pHand dHand deck splits notes bet :as gm]]
;  (print "you shouldn't be here now")
  (let [nonBJHands (dealerhands gm) pTot (BJHandVal pHand)] 
   (average-over-vec (for [hand nonBJHands]
                    (deal-e5 [pTot hand deck (if (and (= (count pHand) 2) (= 21 (BJHandVal pHand)) (= splits 0)) (conj notes :bj) (disj notes :splitA))])))  ))

(def game1 [[6 10] [8] deck1 0 #{} 0])
(defn pdeal [[pHand dHand deck splits notes bet :as gm]]
  (let [nonBJHands (dealerhands gm) pTot (BJHandVal pHand)]
    (/ (reduce + (map #(deal-e5 [pTot % deck (if (and (= (count pHand) 2) (= 21 (BJHandVal pHand)) (= splits 0)) (conj notes :bj) (disj notes :splitA))]) nonBJHands))  ;r/fold, r/map
       (count nonBJHands))))


(declare hit stand doubledown split surrender hit1 stand1)
(def ops [hit stand doubledown split surrender]) ;split

;for testing new bet
;(declare hit stand doubledown tripledown split surrender hit1 stand1)
;(def ops [hit stand doubledown tripledown split surrender]) ;split


(reset! discount 0)
(defn-memo dispatch [[pHand dHand deck splits notes bet :as gm]]
  ;(print "dispatch")
  ;(swap! discount inc)
;  (if (and (= splits 3) (notes 10)) (print gm)  )
  (if (< (reduce + pHand) 22) (swap! handset conj [pHand splits (filter integer? notes)])) ;dHand  ;**what about splits?? we simply don't have outcards recorded here
  (if (not= pHand (sort < pHand)) (swap! nosort inc))
  (doseq [op ops] (op gm)) ;(print (type op)) (print (type gm)));(op gm))
   (last (sort #(< (% 1) (%2 1)) (for [op ops] (op gm)))))

(def splitset #{3})
(def splcnt1 (atom 0))
(def MAXSPLITS 2)
(defn-memo hit [[pHand dHand deck splits notes bet :as gm]] ;(print "pHand: " pHand)
	[:ht (if (and (or (apply not= pHand) (or (= splits 0) (= splits MAXSPLITS)) ) (not (notes :splitA)) ) ;:splitA     **WHY splits=MAX??
        ;(and (or (splitset splits) (apply not= pHand)) (not (notes :splitA)))
           (doseqfor average-over-vec deck #(let [newdeck (rmCard deck1 %) newpHand (addCard pHand %)]  ;(update-in out-map [%] inc) (update-in deck [%] deck); where we then merge-by-??
                             (if (> (BJHandVal newpHand) 21)
						                     -1
                                  ((dispatch [newpHand dHand deck1 splits notes bet]) 1))))
           -10000) ]) 

(defn-memo stand [[pHand dHand deck splits notes bet :as gm]] 
  [:st (if (or (notes :splitA) (apply not= pHand) (or (= splits 0) (= splits MAXSPLITS)) ) ; (not (and (apply = pHand) (< splits 3) )) ) ;(or (splitset splits) (apply not= pHand) (notes :splitA))   
         ;must stand if split A  ;huh, yeah, this does the same thing... so, split AA's should always stand?? 
         ;(do (if (= splits 4) (swap! splcnt1 inc))
           (if (and (= 0 splits) (= pHand [1 10])) 
             1.5
          ;   (- (- 2 (* 2 (lut pHand (first dHand)) )) 1))  
          ;(d-cum-win gm)   ;this lut call uses actual -hand, not -tot
           (- (* 2
                 (d-cum-win gm)
                 ;(lut (BJHandVal pHand) (first dHand))
                 ) 1)	
           ;((standing (if (< (BJHandVal pHand) 17) 0 (- (BJHandVal pHand) 16) )) (first dHand) )
           )
           -10000)])

(def splcnt (atom 0))
(declare splitting) ;lut below
(defn-memo split [[pHand dHand deck splits notes bet :as gm]]  ;change cnstr to apply=??
  (swap! splcnt inc)
  [:sp (if (and (= 2 (count pHand)) (= (pHand 0) (pHand 1)) (< splits MAXSPLITS) (not (notes :splitA))) ;(not (notes :splitA))   ;(not (and (> splits 0) (= (pHand 0) 1))) ) ;splits < 3
         (do (if (= splits 4) (swap! splcnt inc))
           (doseqfor average-over-vec deck #(let [newdeck (rmCard deck1 %) newpHand (vec (sort < [(pHand 0) %]))]  ;**we're dealing all cards here... this isn't just forcing the next split!! BUT it also shouldn't == -20000
                                             (* 2 ((dispatch [newpHand dHand deck1 (inc splits) (if (= (pHand 0) 1) (conj notes :splitA) (conj notes (pHand 0)) ) bet]) 1)) )) ;** (/ ... 2)   ;added (conj (pHand 0) notes) to try and figure out # of splits    ;(conj notes (pHand 0))
           ;((splitting (- (BJHandVal [(pHand 0)]) 2)) (- (BJHandVal [(dHand 0)]) 2))
           )
         -10000)] )

(defn-memo doubledown [[pHand dHand deck splits notes bet :as gm]]
  [:DD (if (and  (= (count pHand) 2) (not (notes :splitA)) (or (apply not= pHand) (or (= splits 0) (= splits MAXSPLITS)) ) ) ;sometimes no DD after split   ;(or (splitset splits) (apply not= pHand))
            (doseqfor average-over-vec deck #(let [newdeck (rmCard deck1 %) newpHand (addCard pHand %)]
                           (if (> (BJHandVal newpHand) 21)
						                   -2
;                                (- (- 4 (* 4 (lut (BJHandVal pHand) (first dHand)) )) 2) )))  ;(d-cum-win [newpHand dHand deck1 splits notes bet])
                                 (- (* 4
                                       (d-cum-win [newpHand dHand deck splits notes bet] )
                                       ;(lut (BJHandVal newpHand) (first dHand))
                                       ) 2) )))  ;(d-cum-win [newpHand dHand deck1 splits notes bet])
         -10000)] )

(defn-memo surrender [[pHand dHand deck splits notes bet :as gm]] [:sr (if (and (not (notes :splitA)) (= 0 splits) (= (count pHand) 2)) -0.5 -10000)]) ;-.75 ?? half your bet + all the dealer's bet ??

(reset! pCount 0)
(print "discount: " discount " splcnt: " splcnt)


(def bjs (atom 0))
(defn from-the-top []
  (print "the top")
  (doseq [pHand combos i deck1] (dispatch [pHand [i] deck1 0 #{} 0]))  ;(filter #(not= % [1 10]) combos)
  (/ (reduce + (for [pHand combos i deck1 j deck1] ;r/fold
                 (cond (= [1 10] (sort [i j])) (do (swap! bjs inc) (if (= pHand [1 10]) 0 -1))
                       :else ((dispatch [pHand [i] deck1 0 #{} 0]) 1))))  ;getting the right i's??
     (* 13 13 169)))

;new from-the-top for new process
; 		(pl*%nonBJ) + .5 * %BJ*%plBJ + 1.5*%pBJ*%nonBJ ??
(defn from-the-top1 []
  (doseq [pHand combos i deck1]
    (swap! pCount inc)
    (dispatch [pHand [i] deck1 0 #{} 0])) ;(vec (reverse ))
  (+ (/ (reduce + ;(vec (concat 
                (for [pHand (filter #(not= % [1 10]) combos) i dDistro] ((dispatch [pHand [i] deck1 0 #{} 0]) 1)))
        (* 169 169)) (* 1.5 8/169 161/169)  (* -1 8/169 161/169)) ;(* 0.5 8/169 8/169)
) ;is the combo count right (have we taken out the BJs?)
    ;eventually need a way to express these given full deck !!!















