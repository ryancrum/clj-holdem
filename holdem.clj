(def *suits* [:hearts :diamonds :clubs :spades])
(def *values* [:ace
               :two :three :four :five :six :seven :eight :nine :ten
               :jack :queen :king])

(def *ordinal-values* (conj *values* :ace))

(def *ordinalities*
     (apply merge
            (map
             (fn [v]
               {v (indices
                   (zipmap (range (count *ordinal-values*))
                           *ordinal-values*) #(= v %))})
             *values*)))

(defstruct card :value :suit)

(defn indices
  ([mp pred] (indices mp pred []))
  ([mp pred res]
     (if (seq mp)
       (if (pred (val (first mp)))
         (recur (rest mp) pred (conj res (key (first mp))))
         (recur (rest mp) pred res))
       res)))

(defn shuffle-deck []
  (let [deck
        (for [value *values*
              suit *suits*]
          (struct card value suit))
        shuffle-map (zipmap (for [x (range (count deck))]
                              (rand-int 1000))
                            deck)]
    (map #(shuffle-map %) (sort (keys shuffle-map)))))

(defn group [lst group-by]
  (let [els (map #(% group-by) lst)
        uniq-els (set els)]
    (zipmap uniq-els
            (map #(count (filter (fn [v] (= v %)) els))
                 uniq-els))))

(defn consecutive?
  ([lst]
     (consecutive? lst 1))
  ([lst step]
     (= (take (count lst) (iterate #(+ step %) (first lst)))
        lst)))

(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(defn list-contains? [lst v]
  (> (count (filter #(= v %) lst)) 0))

(defn contains-all? [lst sublst]
  (not (list-contains? (map #(list-contains? lst %) sublst) false)))

(defn to-ordinals [hand]
  (flatten (map
            #(*ordinalities* (% :value))
            hand)))

(defn grouping [coll prop count]
  (let [matched-coll (indices (group coll prop) #(= % count))]
    (if (seq matched-coll)
      (filter #(= (prop %) (first matched-coll)) coll))))

(defn two-of-a-kind? [hand]
  (grouping hand :value 2))

(defn three-of-a-kind? [hand]
  (grouping hand :value 3))

(defn four-of-a-kind? [hand]
  (grouping hand :value 4))

(defn flush? [hand]
  (grouping hand :suit 5))

(defn straight? [hand]
  (let [hand-vals (distinct (reverse (sort (to-ordinals hand))))]
    (loop [hand-remaining hand-vals]
      (if (< (count hand-remaining)
             5)
        nil
        (if (consecutive? (take 5 hand-remaining) -1)
          (filter (fn [card] (list-contains? 
                               (map #(nth *ordinal-values* %)
                                    (take 5 hand-remaining))
                               (:value card)))
                  hand)
          (recur (rest hand-remaining)))))))

(defn straight-flush? [hand]
  (flush? (straight? hand)))

(defn royal-flush? [hand]
  (let [sflush (straight-flush? hand)]
    (if (and sflush
             (contains-all? (map #(% :value) hand)
                            [:ten :jack :queen :king :ace]))
      sflush)))

(defn full-house? [hand]
  (let [two-kind (two-of-a-kind? hand)
        three-kind (three-of-a-kind? hand)]
    (if (and two-kind three-kind)
      (concat two-kind three-kind))))

(defn two-pair? [hand]
  (<= 4 (count (two-of-a-kind? hand))))

(defn high-card? [hand]
  (last (sort (to-ordinals hand))))

(def *hand-ranks* [high-card?
                    two-of-a-kind?
                    two-pair?
                    three-of-a-kind?
                    straight?
                    flush?
                    full-house?
                    four-of-a-kind?
                    straight-flush?
                    royal-flush?])

(defn last-index-where
  ([lst pred]
     (last-index-where lst pred 0 nil))
  ([lst pred ctr ret]
     (if (seq lst)
       (recur (rest lst)
              pred
              (inc ctr)
              (if (pred (first lst)) ctr ret))
       ret)))

(defn score [hand]
  (last-index-where *hand-ranks* #(% hand)))

(defn winner [hands]
  (zipmap hands (map score hands)))

(defn draw-n [n lst]
  (for [x (range n)]
    (take-nth n (drop x lst))))

(defn deal [player-count]
  (let [deck (shuffle-deck)]
    {:hands (map #(take 2 %) (draw-n player-count deck))
     :deck (drop (* 2 player-count) deck)}))

(defn play-texas-holdem [player-count]
   (let [{hands :hands deck :deck} (deal player-count)]
           (winner (map #(concat (take 5 deck) %) hands))))