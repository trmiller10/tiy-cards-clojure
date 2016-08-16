(ns class-card-clojure.core)

(def suits [:clubs :spades :hearts :diamonds])
;range includes first value, excludes second

(def ranks (range 1 14))

(defn create-deck []
  ;create a set of unique items
  (set
    ;for the bindings of hashmaps suits (defined above) to a suit, ranks to rank
    (for [suit suits
          rank ranks]
      ;create a hashmap with a keyword :suit, :rank, for each suit and rank
      ;ex: {:suit :clubs, :rank 1}
      {:suit suit
       :rank rank})))

;disjoint returns a list where card 1(2,3,4) has been removed from the deck
(defn create-hands [deck] (set
                            (for
                              [card1 deck
                               card2 (disj deck card1)
                               card3 (disj deck card1 card2)
                               card4 (disj deck card1 card2 card3)]
                              #{card1 card2 card3 card4})))
                              ;hand ex: 
                              ;#{{:suit :spades, :rank 6} 
                              ;{:suit :hearts, :rank 11} 
                              ;{:suit :diamonds, :rank 11} 
                              ;{:suit :diamonds, :rank 13}}

(def global-deck (create-deck))

(def global-hands (create-hands global-deck))

(def test-flush-hand #{{:suit :spades, :rank 1} {:suit :hearts, :rank 1} {:suit :clubs, :rank 1} {:suit :diamonds, :rank 1}})

(def test-two-pair-hand-four #{{:suit :spades, :rank 1} {:suit :hearts, :rank 1} {:suit :clubs, :rank 3} {:suit :diamonds, :rank 3}})
(def test-two-pair-hand-three #{{:suit :spades, :rank 4} {:suit :hearts, :rank 4} {:suit :hearts, :rank 3} {:suit :diamonds, :rank 3}})
(def test-two-pair-hand-two #{{:suit :spades, :rank 4} {:suit :hearts, :rank 4} {:suit :hearts, :rank 3} {:suit :spades, :rank 3}})

(def test-straight-hand #{{:suit :spades, :rank 4} {:suit :spades, :rank 2} {:suit :spades, :rank 1} {:suit :hearts, :rank 3}})

(def test-straight-flush-hand  #{{:suit :spades, :rank 4} {:suit :spades, :rank 2} {:suit :spades, :rank 1} {:suit :spades, :rank 3}})

(def test-three-kind-hand #{{:suit :spades, :rank 8} {:suit :hearts, :rank 8} {:suit :clubs, :rank 8} {:suit :hearts, :rank 2}})

(def test-high-straight-flush-hand  #{{:suit :diamonds, :rank 12} {:suit :diamonds, :rank 13} {:suit :diamonds, :rank 11} {:suit :diamonds, :rank 10}})


;returns a collection of the suits in the hand
(defn get-suits [hand] (map :suit hand))
;returns a collection of the ranks in the hand
(defn get-ranks [hand] (map :rank hand))
;sorts the collection of ranks from a given hand
(defn sort-ranks [hand] (sort (get-ranks hand)))
;sorts the collection of suits from a given hand
(defn sort-suits [hand] (sort (get-suits hand)))

;all cards of the same suit, but not in order
;FLUSH?
(defn same-suit? [hand]
  ;get a count of unique items; set returns only unique items out of map, map returns only suits from cards
  (= 1 (count (set (get-suits hand)))))

;returns true if given cards have same rank
;FOUR-OF-A-KIND?
(defn same-rank? [hand]
  (= 1 (count (set (get-ranks hand)))))


;returns true if given cards' ranks are in sequence
;(defn in-a-sequence? [hand] (or (> (map :rank hand)) (< (map :rank hand))))
(defn in-a-sequence?
  [hand]
  (let [ranks (sort-ranks hand)]
    (if (= 4 (count ranks))
      (apply = 1 (map - (rest ranks) ranks))
      false)))


;returns true if given cards' ranks are in sequence but not in same suit
(defn straight?
  "returns true if given cards' ranks are in sequence but not in same suit"
  [hand]
  (and (in-a-sequence? hand) 
       (not (same-suit? hand))))
  
;returns true if all given cards of same suit, in sequence
(defn straight-flush? [hand]
  (and (= (in-a-sequence? hand) true) (= (same-suit? hand) true) (>= 10 (first (sort-ranks hand)))))


;returns true if only 3 of given cards have the same rank
(defn three-of-a-kind? [hand]
  ;in the given hand
  ;get the rank of each card in the hand
  ;compare the ranks of each card
  ;if there are a total of 3 cards that share the same rank
  ;then return 'true'
  ;else return 'false'
  (and (= 2 (count (distinct (get-ranks hand)))) (= 3 (some #{3} (vals (frequencies (get-ranks hand)))))))

;returns true if 2 of given cards have same rank, and other 2 have same rank
(defn two-pair? [hand]
  (and (= 2 (count (distinct (get-ranks hand)))) (not= 3 (some #{3} (vals (frequencies (get-ranks hand)))))))


 
  

(defn -main []
  (let [deck (create-deck)
        hands (create-hands deck)
        four-kind (filter same-rank? hands)
        straight-flush (filter straight-flush? hands)
        three-kind (filter three-of-a-kind? hands)
        straight (filter straight? hands)
        two-pair (filter two-pair? hands)
        flush (filter same-suit? hands)]
    
    (println (count four-kind))
    (println (count straight-flush))
    (println (count three-kind))
    (println (count straight))
    (println (count two-pair))
    (println (count flush))))