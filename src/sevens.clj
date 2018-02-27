(ns sevens
  (require [clojure.core.match :refer [match]]))

(def cards
  (set
    (concat
      (map (fn [x] { :suit :c, :value x}) (range 1 14))
      (map (fn [x] { :suit :d, :value x}) (range 1 14))
      (map (fn [x] { :suit :h, :value x}) (range 1 14))
      (map (fn [x] { :suit :s, :value x}) (range 1 14)))))

(defn valid-moves
  "Find valid moves from played cards"
  [p]
  (match (count p)
         0 #{ {:suit :d, :value 7 } }
         :else (set
                 (concat
                   (mapcat
                     (fn [c] [ (update c :value inc) (update c :value dec) ])
                     p)
                   #{ {:suit :c, :value 7} {:suit :h, :value 7} {:suit :s, :value 7} }
                 ))))

(defn available-moves
  "Find available moves from hand and played cards"
  [h p]
  (clojure.set/intersection (valid-moves p) h))

(defn get-player
  ""
  [state i]
  (nth (state :players) i))

(defn get-player-strat
  ""
  [state i]
  (get (get-player state i) :strat))

(defn get-player-hand
  ""
  [state i]
  (get (get-player state i) :hand))

(defn available-moves-for-player
  ""
  [state i]
  (available-moves (get-player-hand state i) (get state :played_cards)))

(defn get-player-move
  ""
  [state i]
  ((get-player-strat state i) (available-moves-for-player state i) (get state :played_cards)))

(defn remove-from-hand
  ""
  [state i card]
  (update-in state [:players i :hand]
             (fn [h] (disj h card))))

(defn add-to-played-cards
  ""
  [state i card]
  (update state :played_cards (fn [p] (conj p card))))

(defn play-card
  ""
  [state i card]
  (match card
    :pass state
    :else (remove-from-hand (add-to-played-cards state i card) i card)))

(defn make-move
  "Make a move given state and a player"
  [state i]
  (match (count (available-moves-for-player state i))
         0 state
         :else (play-card state i (get-player-move state i))))

(defn is-winner
  "Get a winner if there is one"
  [state i]
  (= (count (get-player-hand state i)) 0))

(defn deal
  "Deal the cards"
  [n]
  (partition-all (/ (count cards) n) (shuffle cards)))

(defn random-strat
  "Pick a move out of available moves"
  [moves played_cards]
  (first moves))

(defn new-game
  "Create a new game"
  [n]
  {
   :played_cards #{}
   :players (vec
              (map
                (fn [h] { :hand (set h), :strat random-strat })
                (deal n)))
   })

(defn play-game
  "Play a game to completion"
  [n]
  (loop [s (new-game n) i 0]
    (if (is-winner s i)
      i
      (recur (make-move s i) (mod (inc i) n))
      )))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println (play-game 4)))
