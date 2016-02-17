(ns chess-utils.core
  (:require clojure.string)
  #?(:cljs (:require-macros [chess-utils.macros :refer [attacks-from-sq-simple attacks-from-sq-complex moves-map]])
     :clj (:require [chess-utils.macros :refer [attacks-from-sq-simple attacks-from-sq-complex moves-map]])))


;;; Auxiliary functions

(defn ord [chr]
  #?( :clj  (if (string? chr) (int(first chr)) (int chr))
      :cljs (.charCodeAt chr 0)))

(defn pad
  ([stri] (pad stri 2 \0))
  ([stri width] (pad stri width \0))
  ([stri width pad-char]
    (let [stri (str stri)]
      (if (>= (count stri) width)
        stri
        (str (clojure.string/join "" (repeat (- width (count stri)) pad-char)) stri))))) 


(def xor7 (partial bit-xor 7))

(def xor56 (partial bit-xor 56))

(defn dec8 [n] ( - n 8))

(def inc8 (partial + 8))

(defn make-tups-from-map [map key] (sort #(<= (second %1) (second %2)) (for [sq-to (map key)] [key sq-to])))

(defn parseInt [stri]
  #?(:clj  (Integer/parseInt (str stri))
     :cljs (js/parseInt (str stri))))

(defn abs [n]
  #? (:clj  (java.lang.Math/abs n)
      :cljs (js/Math.abs n)))

(defn new-date [] #?(:clj (java.util.Date.) :cljs (js/Date.)))

(defn camel-to-dash [stri]
  #?(:clj
      (str (Character/toLowerCase (first stri)) (reduce #(if (Character/isUpperCase %2) (str %1 "-" (Character/toLowerCase %2)) 
                                                             (str %1 %2)) "" (rest stri)))
     :cljs
      (str (.toLowerCase (first stri)) (reduce #(if (.match %2 #"[A-Z]") (str %1 "-" (.toLowerCase %2)) (str %1 %2)) "" (rest stri)))))

(defn dash-to-camel [stri]
  (loop [[h & t] stri, must-upper? true, result ""]
    (cond 
      (empty? t) (str result (if must-upper? (clojure.string/upper-case h) (clojure.string/lower-case h)))
      (= (str h) "-") (recur t true result)
      :else (recur t false (str result (if must-upper? (clojure.string/upper-case h) (clojure.string/lower-case h)))))))
      
(def kw-to-symbol (comp symbol (partial apply str) rest str))

;;; Chess related functions

(comment
fen-doc = "
A FEN record contains six fields. The separator between fields is a space. The fields are:

Piece placement (from white's perspective). Each rank is described, starting with rank 8 and ending with rank 1; within each rank, the contents of each square are described from file "a" through file "h". Following the Standard Algebraic Notation (SAN), each piece is identified by a single letter taken from the standard English names (pawn = "P", knight = "N", bishop = "B", rook = "R", queen = "Q" and king = "K").[1] White pieces are designated using upper-case letters ("PNBRQK") while black pieces use lowercase ("pnbrqk"). Blank squares are noted using digits 1 through 8 (the number of blank squares), and "/" separates ranks.

Active color. "w" means white moves next, "b" means black.

Castling availability. If neither side can castle, this is "-". Otherwise, this has one or more letters: "K" (White can castle kingside), "Q" (White can castle queenside), "k" (Black can castle kingside), and/or "q" (Black can castle queenside).

En passant target square in algebraic notation. If there's no en passant target square, this is "-". If a pawn has just made a two-square move, this is the position "behind" the pawn. This is recorded regardless of whether there is a pawn in position to make an en passant capture.[2]

Halfmove clock: This is the number of halfmoves since the last pawn advance or capture. This is used to determine if a draw can be claimed under the fifty-move rule.

Fullmove number: The number of the full move. It starts at 1, and is incremented after Black's move.
"
  )

(def w-figures #{\K \Q \R \B \N \P})
(def b-figures #{\k \q \r \b \n \p})
(def all-figures (concat w-figures b-figures))
(def kw-figures ["K" "Q"  "R"  "B"  "N" "P" "k" "q" "r" "b" "n" "p"])
(def unicode-figures [\u2654 \u2655 \u2656 \u2657 \u2658 \u2659 \u265A \u265B \u265C \u265D \u265E \u265F])
(def html-figures (map #(str "&#" % ";") (range 9812 9824)))
(def unicode-map (apply assoc {} (interleave kw-figures unicode-figures)))
(def html-map (apply assoc {} (interleave kw-figures html-figures)))
(def figures-map (reduce conj  (map #(hash-map %1 {:u %2 :h %3}) kw-figures unicode-figures html-figures)))


(defn date-to-pgn #?(:clj [^java.util.Date date] :cljs [date])
  #? (:cljs (str (.getFullYear date) "." (pad (inc (.getMonth date))) "." (pad (.getDate date)))
      :clj  (str (+ 1900 (.getYear date)) "." (pad (inc (.getMonth date))) "." (pad (.getDate date)))))

(defn time-to-pgn [d]
  (str (pad (.getHours d)) ":" (pad (.getMinutes d)) ":00"))

;;

(def fen-parts [:position :side-to-move :castling :en-passant :half-move-clock :full-move-number])

(def sicilian-fen-string  "rnb1kbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
(def default-fen-string "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(def empty-fen-string "8/8/8/8/8/8/8/8 w KQkq - 0 1")

(defn fen-to-parts 
  ([] (fen-to-parts default-fen-string))
  ([fen-str] (apply hash-map (interleave fen-parts (clojure.string/split fen-str #"\s+")))))

(defn parts-to-fen [parts] (clojure.string/join " " parts))

(defn expand-pp 
  ([] (expand-pp (:position (fen-to-parts))))
  ([short-pp]
    (-> short-pp 
        (clojure.string/replace #"\d" #(clojure.string/join "" (repeat (parseInt %) "0")))
        (clojure.string/replace "/" ""))))

(defn compress-pp [long-pp]
 (-> long-pp 
     (clojure.string/replace #"(\w{8})(?!\b)" "$1/") 
     (clojure.string/replace #"0+" #(str (count %)))))

(defn fen-pos-to-array ^ints [^String fen-pos]
  (vec (map #(nth fen-pos (xor56 %)) (range 64))))

(defn array-to-fen-pos ^String [^ints array]
  (apply str (fen-pos-to-array array)))

(defn row  [sq] (int (/ sq 8)))

(defn col [sq] (mod sq 8))

(defn sq-from-row-col [r c] (+ (* r 8) c))

(defn is-same-row? [sq1 sq2] (= (row sq1) (row sq2)))

(defn is-same-col? [sq1 sq2] (= (col sq1) (col sq2)))

(defn ^long dif-row [^long sq1 ^long sq2] (abs (- (row sq1) (row sq2))))

(defn ^long dif-col [^long sq1 ^long sq2] (abs (- (col sq1) (col sq2))))

(defn ^{:author "D.E. Savoretti" :doc "Determines if 2 squares are diagonally oriented."} 
      is-diagonal? [sq1 sq2] (= (dif-row sq1 sq2) (dif-col sq1 sq2)))

(defn  is-anti-diagonal? [sq1 sq2] 
  (every? #{true} [(is-diagonal? sq1 sq2) (zero? (mod (abs (- sq1 sq2)) 7)) (and (not= sq1 63) (not= sq2 63))]))

(defn  is-sliding? [sq1 sq2] (or (is-diagonal? sq1 sq2) (is-same-row? sq1 sq2) (is-same-col? sq1 sq2)))

(defn is-knight-jump? [sq1 sq2]
    (or 
      (and (= 1 (dif-row sq1 sq2)) (= 2 (dif-col sq1 sq2)))
      (and (= 2 (dif-row sq1 sq2)) (= 1 (dif-col sq1 sq2)))))

(defn is-adjacent? [sq1, sq2] (and (< (dif-row sq1 sq2) 2) (< (dif-col sq1 sq2) 2)))

(defn path [sq1 sq2]
  (if (not (is-sliding? sq1 sq2))
    []
    (let [[sq-from sq-to] (if (<= sq1 sq2) [sq1 sq2] [sq2 sq1])
          step (cond (is-same-row? sq1 sq2) 1 (is-same-col? sq1 sq2) 8 (is-anti-diagonal? sq1 sq2) 7 :else 9)]
      (vec (range sq-from (inc sq-to) step)))))

(defn is-empty-path? [p] (zero? (count p)))

(defn inner-path [sq1 sq2] (or (vec (butlast (rest (path sq1 sq2)))) []))

;;

(attacks-from-sq-simple k-attacks-from-sq is-adjacent?)
(attacks-from-sq-simple q-attacks-from-sq is-sliding?)
(attacks-from-sq-simple n-attacks-from-sq is-knight-jump?)
(attacks-from-sq-simple b-attacks-from-sq is-diagonal?)

(attacks-from-sq-complex r-attacks-from-sq some #{true} [juxt is-same-row? is-same-col?] (fn [excl] (remove #{excl} (range 64))))
(attacks-from-sq-complex P-attacks-from-sq every? #{true} [juxt (fn [sq1 sq2] 
                                                                  (= 1 (dif-col sq1 sq2))) (fn [sq1 sq2] (= (row sq1) (dec (row sq2))))]
                                                                  (fn [excl] (remove #{excl} (range 16 64))))                                           
(attacks-from-sq-complex p-attacks-from-sq every? #{true} [juxt (fn [sq1 sq2] 
                                                                  (= 1 (dif-col sq1 sq2))) (fn [sq1 sq2] (= (row sq1) (inc (row sq2))))]
                                                                  (fn [excl] (remove #{excl} (range 0 48))))                                           
(defn P-moves-from-sq [sq] (if (= (row sq) 1) #{(+ sq 8) (+ sq 16)} #{(+ sq 8)}))
(defn p-moves-from-sq [sq] (if (= (row sq) 6) #{(- sq 8) (- sq 16)} #{(- sq 8)}))


(moves-map k-attacks #'k-attacks-from-sq (range 64))
(moves-map q-attacks #'q-attacks-from-sq (range 64))
(moves-map n-attacks #'n-attacks-from-sq (range 64))
(moves-map b-attacks #'b-attacks-from-sq (range 64))
(moves-map r-attacks #'r-attacks-from-sq (range 64))
(moves-map P-attacks #'P-attacks-from-sq (range 8 56)) 
(moves-map p-attacks #'p-attacks-from-sq (range 8 56))
(moves-map P-moves #'P-moves-from-sq (range 8 56))
(moves-map p-moves #'p-moves-from-sq (range 8 56))


(def figure-moves-map {
    "k" k-attacks
    "q" q-attacks
    "r" r-attacks
    "b" b-attacks
    "n" n-attacks
    "P-a" P-attacks
    "P-m" P-moves
    "p-a" p-attacks
    "p-m" p-moves
    "P" (into {} (for [k (keys P-attacks)] {k (set (concat (P-attacks k) (P-moves k)))}))
    "p" (into {} (for [k (keys p-attacks)] {k (set (concat (p-attacks k) (p-moves k)))}))
    "0" #{}
    })


(comment
(def k-count (count (flatten (map vec (vals k-attacks)))))
(def q-count (count (flatten (map vec (vals q-attacks)))))
(def n-count (count (flatten (map vec (vals n-attacks)))))
(def b-count (count (flatten (map vec (vals b-attacks)))))
(def r-count (count (flatten (map vec (vals r-attacks)))))
(def P-count (count (flatten (map vec (vals P-attacks)))))
(def p-count (count (flatten (map vec (vals p-attacks)))))
(def P-m-count (count (flatten (map vec (vals P-moves)))))
(def p-m-count (count (flatten (map vec (vals p-moves)))))

(def all-attacks-count (+ k-count q-count n-count b-count r-count P-count p-count))
(def all-moves-count (+ P-m-count p-m-count))
(def all-total-count (+ all-attacks-count all-moves-count))
)

(defn col-to-char [col] (char (+ 97 col)))

(defn char-to-col  [chr] (- (ord (first (str chr)))  97))

(defn sq-to-san [sq] (str (char (+(col sq) 97)) (inc (row sq))))

(defn san-to-sq [san] 
  (if (not= 2 (count san))
    -1
    (sq-from-row-col (- (ord (second san)) 49) (- (ord (first san)) 97))))

(def san-squares (map #(vector % (sq-to-san %)) (range 64)))

(defn sq-color [sq] (let [c-r [(col sq) (row sq)]] (if (or (every? odd? c-r) (every? even? c-r)) "b" "w")))

(def sq-colors (map #(vector % (sq-color %)) (range 64)))  

(defn str-to-coords [stri]
  (re-matches #"([a-h][1-8])[\-x\s]*([a-h][1-8])=?([NBRQ])?[+#]?[!?]*" stri))

;;;


(defrecord Fen [position side-to-moves castling, en-passant half-move-clock full-move-number
                arr P N B R Q K p n b r q k w-army b-army all-army empty-sqs
                P-a N-a B-a R-a Q-a K-a p-a n-a b-a r-a q-a k-a w-attacks b-attacks
                w-moves b-moves w-attacks-on-sqs b-attacks-on-sqs])


(defprotocol ChessRules
  "Collection of funtions aimed at evaluating chess positions and chess games"
  (is-clear-path? [fen-obj sq-from sq-to] "Determines if there are no figures in a given path")
  (king-sq [fen-or-game-obj color] "It's just good to know where the king is located")
  (is-foe? [fen-obj sq1 sq2] "To see if you're going for the enemy")
  (is-empty-sq? [fen-obj sq] "Looking for clear places...")
  (is-friend? [fen-obj sq1 sq2] "When you need help, better rely on a friend")
  (is-not-friend? [fen-obj sq1 sq2] "Searching for territory to occupy...")
  (can-reach? [game-or-fen-obj from to] "Determines if square 'to' is under attack by square 'from'")
  (can-move? [game-or-fen-obj from to] "Determines if a figure can be moved to square 'to' from square 'from'")
  (gen-moves! [fen-obj] "Generates maps of moves for a given position")
  (attacks-from-sq [fen-obj sq] "Returns a vector of squares that can be potentially attacked from the launching square")
  (moves-from-sq [fen-obj sq] "Returns a vector of squares that can effectively receive a figure from the launching square")
  (attacks-on-sq [fen-obj color sq] "To determine which 'color' figures can potentially land on 'sq' ")
  (moves-on-sq [fen-obj color sq] "To determine which 'color' figures can move to given square")
  (moves-on-path [fen-obj color path] "To determine which 'color' figures can land on any square included in path")
  (checks [game-obj] [fen-obj color] "Enemy squares that are daring to threaten 'color' king")
  (is-check? [game-obj] [fen-obj color] "To find if 'checks > 0'")
  (is-legal-position? [fen] "True if side that hasn't got the turn isn't being checked.")  
  (movers [fen] "Helper function to retrieve possible moves from side which has the right to move in the current position")
  (fig-movers [fen figure] "Same as above, but filtered for given figure") 
  (san-to-coords [fen move-str] "Receives a move string in SAN format and tranlates it to proper coords")
  (coords-to-san [fen move-str] [fen from to] [fen from to crowning] "The other way around ;-) ")
  (move [fen move-str] [fen from to] [fen from to crowning] "Overloaded method for generating another Fen through a move applied to current Fen.")
  (has-valid-moves? [game-obj] [fen-obj color] "Quick routine to see if there's anything left to do.")
  (legal-moves [game-obj] [fen-obj color] "Array of movements that happen to give birth to a legal position")
  (is-check-mate? [game-obj] [fen color] "To know if this is kaput") 
  (is-stale-mate? [game-obj] [fen color] "To know if the other side has been dumb enough to let you without options while not being checked")
  (to-string [fen-or-game] "String repr of the damned thing.")
)

(declare make-fen)

(extend-protocol ChessRules
  Fen
  (is-clear-path? [fen-obj sq-from sq-to] (or (is-empty-path? (path sq-from sq-to)) (every? #(= \0 (nth (:arr fen-obj) %)) (inner-path sq-from sq-to))))
  
  (king-sq [fen-obj color] (first (if (= color "w") (:K fen-obj) (:k fen-obj))))
  
  (is-foe? [fen-obj sq1 sq2] (boolean (or (and (w-figures (nth (:arr fen-obj) sq1)) (b-figures (nth (:arr fen-obj) sq2)))
                                 (and (b-figures (nth (:arr fen-obj) sq1)) (w-figures (nth (:arr fen-obj) sq2))))))
  
  (is-empty-sq? [fen-obj sq] (= (nth (:arr fen-obj) sq) \0))
  
  (is-friend? [fen-obj sq1 sq2] (and (not (is-foe? fen-obj sq1 sq2)) (not (is-empty-sq? fen-obj sq2))))
  
  (is-not-friend? [fen-obj sq1 sq2] ((complement is-friend?) fen-obj sq1 sq2))
  
  (can-reach? [fen-obj from to]
    (let [figure (nth (:arr fen-obj) from)
          lfigure (clojure.string/lower-case figure)
          m-set (cond 
                  (= \P figure) ((figure-moves-map "P-a") from)
                  (= \p figure) ((figure-moves-map "p-a") from)
                  :else ((figure-moves-map lfigure) from))]
      (cond 
        (not (m-set to)) false
        :else (is-clear-path? fen-obj from to))))
      
 (can-move? [fen-obj from to]
    (let [figure (nth (:arr fen-obj) from)
          lfigure (clojure.string/lower-case figure)
          m-set (cond 
                  (= \P figure) ((figure-moves-map "P") from)
                  (= \p figure) ((figure-moves-map "p") from)
                  :else ((figure-moves-map lfigure) from))]
      (cond
        (and (= figure \K) (= from 4) (= to 6)) (if (and (re-matches #".*K.*" (:castling fen-obj)) (is-clear-path? fen-obj 4 7)) true false)   
        (and (= figure \K) (= from 4) (= to 2)) (if (and (re-matches #".*Q.*" (:castling fen-obj)) (is-clear-path? fen-obj 4 0)) true false)   
        (and (= figure \k) (= from 60) (= to 62)) (if (and (re-matches #".*k.*" (:castling fen-obj)) (is-clear-path? fen-obj 60 63)) true false)   
        (and (= figure \k) (= from 60) (= to 58)) (if (and (re-matches #".*q.*" (:castling fen-obj)) (is-clear-path? fen-obj 60 56)) true false)   
        (not (m-set to)) false
        (is-friend? fen-obj from to) false
        (#{\P\p} figure) (if (= (col from) (col to)) 
                          (is-clear-path? fen-obj from ((if (= figure \P) inc8 dec8) to)) 
                          (or (is-foe? fen-obj from to) (= to (san-to-sq (:en-passant fen-obj)))))
        :else (is-clear-path? fen-obj from to))))
      
 (gen-moves! [fen-obj]
   (letfn [(filter-moves [func fen figure kw]
      (filter #(func fen (first %) (second %)) (mapcat #(make-tups-from-map (figure-moves-map figure) %) (kw fen))))]
    (let [P-a (filter-moves can-reach? fen-obj "P-a" :P) p-a (filter-moves can-reach? fen-obj "p-a" :p)
          N-a (filter-moves can-reach? fen-obj "n" :N) n-a (filter-moves can-reach? fen-obj "n" :n)
          B-a (filter-moves can-reach? fen-obj "b" :B) b-a (filter-moves can-reach? fen-obj "b" :b)
          R-a (filter-moves can-reach? fen-obj "r" :R) r-a (filter-moves can-reach? fen-obj "r" :r)
          Q-a (filter-moves can-reach? fen-obj "q" :Q) q-a (filter-moves can-reach? fen-obj "q" :q)
          K-a (filter-moves can-reach? fen-obj "k" :K) k-a (filter-moves can-reach? fen-obj "k" :k)
          w-attacks (concat P-a N-a B-a R-a Q-a K-a) b-attacks (concat p-a n-a b-a r-a q-a k-a)
          w-moves (mapcat (fn [s] (for [n (moves-from-sq fen-obj s)] [s n])) (:w-army fen-obj))
          b-moves (mapcat (fn [s] (for [n (moves-from-sq fen-obj s)] [s n])) (:b-army fen-obj))
          w-attacks-on-sqs nil
          b-attacks-on-sqs nil
         ]
      ( -> fen-obj
        (assoc :P-a P-a)
        (assoc :N-a N-a)
        (assoc :B-a B-a)
        (assoc :R-a R-a)
        (assoc :Q-a Q-a)
        (assoc :K-a K-a)
        (assoc :p-a p-a)
        (assoc :n-a n-a)
        (assoc :b-a b-a)
        (assoc :r-a r-a)
        (assoc :q-a q-a)
        (assoc :k-a k-a)
        (assoc :w-attacks w-attacks)
        (assoc :b-attacks b-attacks)
        (assoc :w-attacks-on-sqs w-attacks-on-sqs)
        (assoc :b-attacks-on-sqs b-attacks-on-sqs)
        (assoc :w-moves w-moves)
        (assoc :b-moves b-moves)))))
  
  (moves-from-sq [fen-obj sq]
    (condp  = (nth (:arr fen-obj) sq)
      \0 []
      \P (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "P") sq)))
      \p (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "p") sq)))
      \N (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "n") sq)))
      \n (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "n") sq)))
      \B (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "b") sq)))
      \b (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "b") sq)))
      \R (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "r") sq)))
      \r (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "r") sq)))
      \Q (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "q") sq)))
      \q (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "q") sq)))
      \K (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "k") sq)))
      \k (vec (filter #(can-move? fen-obj sq %) ((figure-moves-map "k") sq)))))
  
  (attacks-from-sq [fen-obj sq]
    (let [figure (nth (:arr fen-obj) sq) 
          attackers (cond 
                      (w-figures figure) (:w-attacks fen-obj) 
                      (b-figures figure) (:b-attacks fen-obj) 
                      :else [])]
      (vec (filter #(= (first %) sq) attackers))))

  (attacks-on-sq [fen-obj color sq]
    (let [attackers (if (= color "w") (:w-attacks fen-obj) (:b-attacks fen-obj))]
      (vec (filter #(= (second %) sq) attackers))))

  (moves-on-sq [fen-obj color sq]
    (let [movers (if (= color "w") (:w-moves fen-obj) (:b-moves fen-obj))]
      (vec (filter #(= (second %) sq) movers))))
           
  (moves-on-path [fen-obj color path] 
     (mapcat #(moves-on-sq fen-obj color %) path)) 
  
  (checks [fen-obj color] (attacks-on-sq fen-obj (if (= color "w") "b" "w") (king-sq fen-obj color)))

  (is-check? [fen-obj color] (not (zero? (count (checks fen-obj color)))))
  
  (is-legal-position? [fen-obj] (let [not-moving (if (= (:side-to-move fen-obj) "w") "b" "w")] (not (is-check? fen-obj not-moving))))
  
  (san-to-coords [fen move-str]
    (let [pgn-move-regex-pat (re-pattern (str "(?:(^0-0-0|^O-O-O)|(^0-0|^O-O)|"
                               "(?:^([a-h])(?:([1-8])|(?:x?([a-h][1-8])))(?:=?([NBRQ]))?)|"
                               "(?:^([NBRQK])([a-h])?([1-8])?(x)?([a-h][1-8])))(?:(\\+)|(#)|"
                               "(\\+\\+))?[!?]*$"))
          [full-move long-castle short-castle pawn-col-char pawn-row-num pawn-dest-sq crowning
           figure-moved extra-col-info extra-row-info capture figure-destiny check check-mate-numeral 
           check-mate-double-plus]  (re-matches pgn-move-regex-pat move-str)
           ;_ (.log js/console (str "Recibido esto como 'move-str': " move-str " --- "))    
          who-moves (:side-to-move fen)]
      (cond
       (nil? full-move) [nil nil nil]
       (not (nil? long-castle)) (if (= who-moves "w") [4 2 nil] [60 58 nil])
       (not (nil? short-castle)) (if (= who-moves "w") [4 6 nil] [60 62 nil])
       (not (nil? pawn-col-char)) (let [army (if (= who-moves "w") (:P fen) (:p fen))
                                        column (char-to-col pawn-col-char)
                                        sq-to (if pawn-row-num (san-to-sq (str pawn-col-char pawn-row-num)) (san-to-sq pawn-dest-sq))
                                        candidates (filter #(can-move? fen % sq-to) (filter #(= (col %) column) army))
                                        sq-from (if (= (count candidates) 1)
                                                    (first candidates)
                                                    nil)]
                                    [sq-from (if (nil? sq-from) nil sq-to) crowning])
       (not (nil? figure-moved)) (let [army-prefix (if (= who-moves "w") (clojure.string/upper-case figure-moved) (clojure.string/lower-case figure-moved))
                                      ;army-suffix (utils/lower-case figure-moved)
                                      ;army ((keyword (str army-prefix army-suffix)) fen-obj)
                                      army ((keyword (str army-prefix)) fen)
                                      sq-to (san-to-sq figure-destiny)
                                      candidates (filter #(can-move? fen % sq-to) army)
                                      sq-from (if (= (count candidates) 1)
                                                  (first candidates)
                                                  (cond
                                                    (and (not (nil? extra-col-info)) (not (nil? extra-row-info)))
                                                      (let [sq (san-to-sq (str extra-col-info extra-row-info))] (first (filter #(= % sq) candidates)))
                                                    (and (nil? extra-col-info) (nil? extra-row-info))
                                                      (let [f-cands (filter #(is-legal-position? (move fen % sq-to)) candidates)]
                                                        (if (= (count f-cands) 1) (first f-cands) nil))
                                                    (not (nil? extra-col-info))
                                                      (let [column (char-to-col extra-col-info) f-cands (filter #(= (col %) column) candidates)]
                                                        (if (= (count f-cands) 1) (first f-cands) nil))
                                                    (not (nil? extra-row-info))
                                                      (let [the-row (dec (parseInt extra-row-info)) f-cands (filter #(= (row %) the-row) candidates)]
                                                        (if (= (count f-cands) 1) (first f-cands) nil))
                                                    :else nil))]
                                    [sq-from (if (nil? sq-from) nil sq-to) nil])

       :else [nil nil nil])))

  (movers [fen] (if (= (:side-to-move fen) "w") (:w-moves fen) (:b-moves fen)))
  
  (fig-movers [fen figure]
    (let [army (if (= (:side-to-move fen) "w") (set ((keyword (clojure.string/upper-case figure)) fen)) (set ((keyword (clojure.string/lower-case figure)) fen)))]
       (filter #(army (first %)) (movers fen))))      
              
  
  (coords-to-san
    ([fen-obj movestr]
      (let [[_ san-from san-to crowning] (re-matches #"([a-h][1-8]).?([a-h][1-8])=?([NBRQ])?[+#]?" movestr)]
        (if (and san-from san-to)
          (coords-to-san fen-obj (san-to-sq san-from) (san-to-sq san-to) crowning)
          (let [[from to crowning] (san-to-coords fen-obj movestr)] (if from (coords-to-san fen-obj from to crowning) nil)))))
    ([fen-obj from to](coords-to-san fen-obj from to nil))
    ([fen-obj from to crowning]
      (let [figure-from  ((:arr fen-obj) from)
            figure-to ((:arr fen-obj) to)
            capture (if (or (is-foe? fen-obj from to) (and (#{\P\p} figure-from) (= to (san-to-sq (:en-passant fen-obj))))) "x" "")
            upper-figure-from (if (and (not= figure-from \p) (not= figure-from \P)) 
                                (clojure.string/upper-case figure-from) 
                                (if (and (#{\P\p} figure-from) (= to (san-to-sq (:en-passant fen-obj)))) (str (col-to-char (col from))) ""))
            col-orig (if (and (= capture "x") (= upper-figure-from "")) (str (col-to-char (col from))) "")
            pgn-dest (sq-to-san to)
            str-crowning (if (nil? crowning) "" (str "=" (clojure.string/upper-case crowning)))
            [extra-col-info extra-row-info] (cond
                                              (= upper-figure-from "") ["" ""]
                                              (= upper-figure-from "K") ["" ""]
                                              :else (let [moves (fig-movers fen-obj (str figure-from))
                                                          f1-moves (filter #(= (second %) to) moves)
                                                          f3-moves (filter #(is-legal-position? (move fen-obj (first %) (second %))) f1-moves)
                                                         ]
                                                 (cond
                                                   (< (count f3-moves) 2) ["" ""]
                                                   (> (count f3-moves) 2) [(first (sq-to-san from)) (second (sq-to-san from))]
                                                   :else (if (zero? (dif-col (first (first f3-moves)) (first (second f3-moves))))
                                                             ["" (str (inc (row from)))]
                                                             [(col-to-char (col from)) ""]))))
            new-fen (move fen-obj from to crowning)
            chk (let [target (:side-to-move new-fen)]
                    (cond
                      (nil? new-fen) ""
                      (is-check-mate? new-fen  target) "#"
                      (is-check? new-fen target) "+"
                      :else ""))
       ]
         (if (nil? new-fen)
           nil
           (apply str [upper-figure-from extra-col-info extra-row-info col-orig capture pgn-dest (str str-crowning) chk])))))


  (move 
   ([fen move-str]
    (let [[match san-from san-to crowning] (str-to-coords move-str)] 
      (if match
        (move fen (san-to-sq san-from) (san-to-sq san-to) crowning)
        (let [[from to crowning] (san-to-coords fen move-str)]
          (if from
            (move fen from to crowning)
            nil)))))

   ([fen from to] (move fen from to nil))
   ([fen from to crowning]
     (cond
      (not (can-move? fen from to)) nil
      (and (= (nth (:arr fen) from) \K) (= from 4) (= to 6) ((complement empty?) (mapcat #(attacks-on-sq fen "b" %) (path 4 6)))) nil   
      (and (= (nth (:arr fen) from) \K) (= from 4) (= to 2) ((complement empty?) (mapcat #(attacks-on-sq fen "b" %) (path 4 2)))) nil   
      (and (= (nth (:arr fen) from) \k) (= from 60) (= to 62) ((complement empty?) (mapcat #(attacks-on-sq fen "w" %) (path 60 62)))) nil   
      (and (= (nth (:arr fen) from) \k) (= from 60) (= to 58) ((complement empty?) (mapcat #(attacks-on-sq fen "w" %) (path 60 58)))) nil   
      :else (let [figure-from (nth (:arr fen) from)
                  figure-to (nth (:arr fen) to)
                  is-en-passant-w? (and (= figure-from \P) (= to (san-to-sq (:en-passant fen))))
                  is-en-passant-b? (and (= figure-from \p) (= to (san-to-sq (:en-passant fen))))
                  friend (:side-to-move fen)
                  foe (if (= friend "w") "b" "w")
                  final-figure-to (if-not crowning figure-from (if (= friend "w") (first (clojure.string/upper-case crowning)) 
                                                                                  (first (clojure.string/lower-case crowning))))
                  new-arr (-> (:arr fen)
                              (assoc from \0 to final-figure-to)
                              ((fn [a] (if is-en-passant-w? (assoc a (- to 8) \0) a)))   
                              ((fn [a] (if is-en-passant-b? (assoc a (+ to 8) \0) a)))
                              ((fn [a] (if (and (= figure-from \K) (= from 4) (= to 6)) (assoc a 5 \R 7 \0) a)))   
                              ((fn [a] (if (and (= figure-from \K) (= from 4) (= to 2)) (assoc a 3 \R 0 \0) a)))   
                              ((fn [a] (if (and (= figure-from \k) (= from 60) (= to 62)) (assoc a 61 \r 63 \0) a)))   
                              ((fn [a] (if (and (= figure-from \k) (= from 60) (= to 58)) (assoc a 59 \r 56 \0) a)))   
                          )
                  ;new-side-to-move foe
                  new-castling (-> (:castling fen)
                                   ((fn [cstr] (let [newstr (cond
                                                              (= from 4) (clojure.string/replace cstr #"[KQ]" "")
                                                              (= from 60) (clojure.string/replace cstr #"[kq]" "")
                                                              (= from 0) (clojure.string/replace cstr #"[Q]" "")
                                                              (= from 7) (clojure.string/replace cstr #"[K]" "")
                                                              (= from 56) (clojure.string/replace cstr #"[q]" "")
                                                              (= from 63) (clojure.string/replace cstr #"[k]" "")
                                                              :else cstr)]
                                     (if (= newstr "") "-" newstr))))
                               )
                  new-en-passant (cond 
                                   (and (= figure-from \P) (= (row from) 1) (= (row to) 3)) (sq-to-san (- to 8))
                                   (and (= figure-from \p) (= (row from) 6) (= (row to) 4)) (sq-to-san (+ to 8))
                                   :else "-")
                  new-position (compress-pp (array-to-fen-pos new-arr))
                  ;_ (println new-position)
                  new-half-move-clock (if (or (= (clojure.string/lower-case (str figure-from)) "p") (not (= figure-to \0)))
                                        "0" (str (inc (parseInt (:half-move-clock fen)))))
                  new-full-move-number (if (= friend "w") 
                                         (:full-move-number fen) 
                                         (str (inc (parseInt (:full-move-number fen))))) 
                  ]
              (make-fen (parts-to-fen [new-position foe new-castling new-en-passant new-half-move-clock new-full-move-number]))))))


  (has-valid-moves? [fen-obj color] 
    (let [chks (checks fen-obj color)
          k-sq (king-sq fen-obj color)
          k-m (map #(vector k-sq %) (moves-from-sq fen-obj k-sq))
          moves-to-test (cond 
           (> (count chks) 1) k-m
           (= (count chks) 1) (set (concat k-m
                                      (moves-on-path fen-obj color (inner-path (first (first chks)) k-sq))
                                      (attacks-on-sq fen-obj color (first (first chks))))) 
           
           :else (if (= color "w") (:w-moves fen-obj) (:b-moves fen-obj)))
          ;_ (println moves-to-test)
          ]
      (boolean (some #(is-legal-position? (move fen-obj (first %) (second %) nil)) moves-to-test))))

  (to-string [fen-obj] (clojure.string/join " " [(:position fen-obj) (:side-to-move fen-obj)
                                                 (:castling fen-obj) (:en-passant fen-obj)
                                                 (:half-move-clock fen-obj) (:full-move-number fen-obj)]))

  (is-check-mate? [fen-obj color] (and (is-check? fen-obj color) (not (has-valid-moves? fen-obj color))))
  
  (is-stale-mate? [fen-obj color] (and (not (is-check? fen-obj color)) (not (has-valid-moves? fen-obj color))))

  (legal-moves [fen-obj color] (vec (filter #(is-legal-position? (move fen-obj (first %) (second %))) 
                                                         ((if (= color "w") :w-moves :b-moves) fen-obj))))
      
)

(def decorated-move (juxt coords-to-san move))  


(defn make-simple-fen
  ([] (make-simple-fen default-fen-string))
  ([fen-str]
    (let [proto-fen (map->Fen (fen-to-parts fen-str))
          arr (fen-pos-to-array (expand-pp (:position proto-fen)))
          filterer (fn [figure] (filter #(= figure (nth arr %)) (range 64)))
          P (filterer \P) N (filterer \N) B (filterer \B) R (filterer \R) Q (filterer \Q) K (filterer \K)
          p (filterer \p) n (filterer \n) b (filterer \b) r (filterer \r) q (filterer \q) k (filterer \k) 
          w-army (concat P N B R Q K) b-army (concat p n b r q k) all-army (concat w-army b-army) empty-sqs (remove (set all-army) (range 64))
          ;P-a (filter #() P-attacks)
          evolved-fen (-> proto-fen 
                          (assoc :arr arr) 
                          (assoc :P P) (assoc :N N)  (assoc :B B) (assoc :R R) (assoc :Q Q) (assoc :K K) 
                          (assoc :p p) (assoc :n n)  (assoc :b b) (assoc :r r) (assoc :q q) (assoc :k k)
                          (assoc :w-army w-army) (assoc :b-army b-army) (assoc :all-army all-army) (assoc :empty-sqs empty-sqs))]
    evolved-fen)))

(defmulti make-complex-fen #(type %))
(defmethod make-complex-fen (type "a") [fen-string] (make-complex-fen (make-simple-fen fen-string)))
(defmethod make-complex-fen (type (map->Fen {})) [fen-obj] (gen-moves! fen-obj))
(defmethod make-complex-fen :default [anydin] (do (println "Type " (type anydin) " not supported by 'make-complex-fen'\nYou got 'nil'.") nil))


(def make-fen
  (memoize (fn 
    ([] (make-complex-fen (make-simple-fen)))
    ([fen-str] (make-complex-fen fen-str)))))


(def top-border (apply str (flatten [\u250C (repeat 7 (str \u2500 \u2500 \u2500 \u252C)) \u2500 \u2500 \u2500 \u2510 "\n"])))
(def middle-border (apply str (flatten [\u251C (repeat 7 (str \u2500 \u2500 \u2500 \u253C)) \u2500 \u2500 \u2500 \u2524])))
(def bottom-border (apply str (flatten [\u2514 (repeat 7 (str \u2500 \u2500 \u2500 \u2534)) \u2500 \u2500 \u2500 \u2519 "\n"])))
(def vert-sep \u2502)

(defn ascii-board 
  ([] (ascii-board (make-fen) false))
  ([fen] (ascii-board fen false))
  ([fen use-symbols?]
   (-> (clojure.string/join "\n" (map #(apply str %) (partition 8 (expand-pp (:position fen)))))
      (clojure.string/replace #"0" " ")
      (clojure.string/split #"\n")
      ((fn [splitted] (map #(str vert-sep " " (apply str (interpose (str " " vert-sep " ") (seq %))) " " vert-sep) splitted)))
      ((fn [seps] (interpose middle-border seps)))
      ((fn [lines] (clojure.string/join "\n" lines)))
      ((fn [joined] (if use-symbols? (clojure.string/replace joined #"[KkQqRrBbNnPp]" #(str (unicode-map %))) joined)))
      ((fn [joined] (str top-border joined "\n" bottom-border)))
   )))


;;; Cleaning up protocols

(extend-protocol ChessRules
  nil
    (move [obj anydin] nil)
)

;;; Game part

(def default-game-tags [:event :site :date :round :white :black :result :moves :fens])

(def default-game-other-tags [:annotator :ply-count :time-control :time :termination :mode :fen :eco])

(def all-game-tags (into default-game-tags default-game-other-tags))

(def default-time-control  "40/7200:3600")

(def modes ["ICS" "OTB"])

(def terminations [ "abandoned" "adjudication" "death" "emergency" "normal" "rules infraction" "time forfeit" "unterminated"])

(defn mk-game-prototype []
  (let [d (new-date)]
    {:event "Internet Game"
     :site "Internet"
     :date (date-to-pgn d)
     :round 1
     :white "White Player"
     :black "Black Player"
     :result "*"
     :moves [""]
     :fens [(make-fen)]
     :annotator ""
     :ply-count 0
     :time-control ""
     :time (time-to-pgn d)
     :termination ""
     :mode (modes 0)
     :fen default-fen-string
     :eco "A00"}))

(defrecord Game [event site date round white black result moves fens annotator ply-count time-control time termination mode fen eco])

(defn pad-game [game-obj]
  (first (reduce #(if (nil? (%2 (first %1)))
                    [(assoc (first %1) %2 ((second %1) %2)) (second %1)]
                    [(first %1) (second %1)])
                 [game-obj (mk-game-prototype)]
                 all-game-tags)))

(defmulti make-game (fn [& fields] (type (first fields))))
(defmethod make-game nil [& fields] (map->Game (mk-game-prototype)))
(defmethod make-game (type "s") [& tags]
   (let [d (new-date) [event site date round white black result moves fens annotator ply-count time-control time termination mode fen eco] tags]
     (Game. (or event "Internet game") (or site "Internet") (or date (date-to-pgn d))
            (or round 1) (or white "White Player") (or black "Black Player") (or result "*")
            (or moves [""]) (or fens [(make-fen)]) (or annotator "")
            (or ply-count (if fens (dec (count fens)) 0))
            (or time-control "") (or time (time-to-pgn d)) (or termination "") 
            (or mode (modes 0)) (or fen default-fen-string) (or eco "A00") )))
(defmethod make-game :default [game-obj] (pad-game (map->Game game-obj)))

(def mlist-to-game (memoize (fn [& moves]
  (if (nil? moves)
    (make-game)
    (let [game-obj (if (string? (first moves)) (make-game) (let [g (make-game (first moves))] (assoc g :ply-count 0 :moves [""] :fens [(first (:fens g))])))
          moves-list (if (string? (first moves)) moves (rest moves))]
      (reduce move game-obj moves-list))))))
    
(defn side-to-move [game-obj]
  (:side-to-move (last (:fens game-obj))))

(defn valid-tags [game-obj]
  (loop [[ky & kys] (keys game-obj) result {}]
    (cond
      (empty? kys) (assoc result ky (ky game-obj))
      ((complement nil?) (ky game-obj)) (recur kys (assoc result ky (ky game-obj)))
      :else (recur kys result))))

(defn game-moves-to-string [game-obj]
  (let [pmoves (partition-all 2 (rest (:moves game-obj)))
        rang (range 1 (inc (count pmoves)))
        str-list (map #(str %1 "." (first %2) " " (str (first (rest  %2)))) rang pmoves)
        raw-str (apply str (interpose " " str-list))
        ]
    (apply str (flatten (interpose "\n" (partition-all 80 raw-str))))))

(defn game-tags-to-string [game-obj]
  (let [v-obj (valid-tags game-obj)
        ot-obj (reduce dissoc v-obj default-game-tags)
        ot-prefixes ["[Annotator \"" "[PlyCount \"" "[TimeControl \"" "[Time \"" "[Termination \"" "[Mode \"" "[FEN \"" "[ECO \""]
        tag-suffix "\"]\n"
        init-str (str "[Event \"" (:event v-obj) "\"]\n"
                      "[Site \"" (:site v-obj) "\"]\n"
                      "[Date \"" (:date v-obj) "\"]\n"
                      "[Round \"" (:round v-obj) "\"]\n"
                      "[White \"" (:white v-obj) "\"]\n"
                      "[Black \"" (:black v-obj) "\"]\n"
                      "[Result \"" (:result v-obj) "\"]\n")
        compl-str (loop [okeys default-game-other-tags prefixes ot-prefixes result ""]
                    (cond
                      (empty? okeys) result
                      (not (nil? ((first okeys) ot-obj))) (recur (rest okeys) (rest prefixes) (str result (first prefixes) ((first okeys) ot-obj) tag-suffix))
                      :else (recur (rest okeys) (rest prefixes) result)))
        ]
    (str init-str compl-str "\n")))


(extend-protocol ChessRules
  Game
    (can-reach? [game-obj from to] (can-reach? (last (:fens game-obj)) from to))
    (can-move? [game-obj from to] (can-move? (last (:fens game-obj)) from to))
    (is-legal-position? [game-obj] (is-legal-position? (last (:fens game-obj))))
    (legal-moves [game-obj] (legal-moves (last (:fens game-obj)) (:side-to-move (last (:fens game-obj)))))
    (has-valid-moves? [game-obj] (has-valid-moves? (last (:fens game-obj)) (:side-to-move (last (:fens game-obj)))))
    (checks [game-obj] (checks (last (:fens game-obj)) (:side-to-move (last (:fens game-obj)))))
    (is-check? [game-obj] (is-check? (last (:fens game-obj)) (:side-to-move (last (:fens game-obj)))))
    (is-check-mate? [game-obj] (is-check-mate? (last (:fens game-obj)) (:side-to-move (last (:fens game-obj)))))
    (is-stale-mate? [game-obj] (is-stale-mate? (last (:fens game-obj)) (:side-to-move (last (:fens game-obj)))))
    (move [game movestr]
      (let [fen (last (:fens game)) [sq-from  _ _] (san-to-coords fen movestr) [pgn new-fen] (decorated-move fen movestr)]
        (if (or (nil? sq-from) (nil? pgn) (not  (is-legal-position? new-fen)))
          nil
          (let [pre-game (assoc game :moves (conj (:moves game) pgn)
                                     :fens (conj (:fens game) new-fen)
    	                               :ply-count (inc (parseInt (:ply-count game))))]
            (cond
              (is-check-mate? pre-game) (if (= (side-to-move pre-game) "w") (assoc pre-game :result "0-1") (assoc pre-game :result "1-0"))
              (is-stale-mate? pre-game) (assoc pre-game :result "1/2-1/2")
              :else pre-game)))))
    (to-string [game-obj]
      (str (game-tags-to-string game-obj) (game-moves-to-string game-obj) " " (:result game-obj) "\n\n"))


  nil
    (can-reach? [fen-obj _ _] nil)
    (can-move? [fen-obj _ _] nil)
    (legal-moves [fen-obj _] nil)
    (is-legal-position? [fen-obj] nil)
    (has-valid-moves? [fen-obj _] nil)
    (checks [fen-obj _] nil)
    (is-check? [fen-obj _] nil)
    (is-check-mate? [fen-obj _] nil)
    (is-stale-mate? [fen-obj _] nil)
    (move [fen-obj & args] nil)

 )

(defn print-game [game-obj] (println (to-string game-obj)))


;;;;;;;

(def pgn-clean-moves-regex-pat 
  #?(:clj   #"(\*)|(\d\-\d)|(1\/2\-1\/2)|([\?\!]+)|(\d+\.)|([\n\r]+)|(\s$)"
     :cljs  #"(\*)|(\d\-\d)|(\d+\.)|(1/2-1/2)|([\?\!]+)|([\n\r]+)|(\s$)"))

(def pgn-taglines-regex-pat #"\[\s*(.+?)\s+\"(.+?)\"\s*\]")

(defn make-game-pairs [pgn-str]
  (let [r-pgn-str (-> pgn-str (clojure.string/replace #"\r\n" "\n") (clojure.string/replace #"\r" " "))]
   (partition 2 (clojure.string/split r-pgn-str #"\n{2,}"))))

(defn read-taglines [taglines]
  (let [game-obj (apply assoc {} (flatten
                    (map (fn [v] [(keyword (camel-to-dash (second v))) (last v)])
                      (re-seq pgn-taglines-regex-pat taglines))))]
    (-> game-obj 
        (assoc :fen (game-obj :f-e-n default-fen-string))
        (dissoc :f-e-n)
        (assoc :eco (game-obj :e-c-o "A00"))
        (dissoc :e-c-o))))


(def pgn-move-regex-pat #"(?:(^0-0-0|^O-O-O)|(^0-0|^O-O)|(?:^([a-h])(?:([1-8])|(?:x([a-h][1-8])))(?:=?([NBRQ]))?)|(?:^([NBRQK])([a-h])?([1-8])?(x)?([a-h][1-8])))(?:(\+)|(#)|(\+\+))?$")

(defn read-moves [moves-str]
   (filter #(re-matches pgn-move-regex-pat %) (remove #{""} 
                                              (clojure.string/split (clojure.string/replace moves-str pgn-clean-moves-regex-pat " ") #"[\n\s]+"))))

(defn make-game-obj-pairs [pairs]
  (map (fn [pair] (vector (read-taglines (first pair)) (read-moves (second pair)))) pairs))

(defn read-pgn-str [pgn-str & only-headers]
   (let [pairs (make-game-obj-pairs (make-game-pairs pgn-str))]
     (if only-headers
       (map #(make-game (first %)) pairs)
       (map (fn [pair] (reduce move (let [g (make-game (first pair))] 
                                         (assoc g :ply-count 0 :moves [""] :fens [(first (:fens g))])) (second pair))) pairs))))

(defn game-to-resume [game-obj] (str (:white game-obj) " - " (:black game-obj) "\t" (:result game-obj)))


;;; End of game part



;;; Example fens

(def ^:exports mate-del-loco  "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3")
(def ^:exports sicilian  "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
(def ^:exports scandinavian  "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")
(def ^:exports ayudado  "r1bqnNnr/pppkpp1p/7R/3p4/8/8/PPPPPPP1/RNBQKBN1 b Q - 0 6")
(def ^:exports immortal-moves ["e4" "e5" "f4" "ef4" "Bc4" "Qh4" "Kf1" "b5" "Bb5" "Nf6" "Nf3" "Qh6" "d3" "Nh5" "Nh4" "Qg5" "Nf5" "c6" "g4" "Nf6" 
 "Rg1" "cb5" "h4" "Qg6" "h5" "Qg5" "Qf3" "Ng8" "Bf4" "Qf6" "Nc3" "Bc5" "Nd5" "Qb2" "Bd6" "Bg1" "e5" "Qa1" "Ke2" 
 "Na6" "Ng7" "Kd8" "Qf6" "Nf6" "Be7#"])

(comment
#?(:cljs (do
     (enable-console-print!)
     ;(.log js/console (str "Esto es un string: " ayudado))
     ;(.log js/console (str "Esto es el método to-string del objeto FEN: " (to-string (make-fen ayudado))))
     ;(println (clojure.string/upper-case "mayúsculas"))
     (let [loco (make-fen mate-del-loco)] (do (println (to-string loco)) 
                                              (println "En el mate del loco el blanco recibe jaque mate? " (is-check-mate? loco "w"))))
    )
         
   :clj (let [immortal (reduce move (make-fen) immortal-moves)] 
         (do (println "Immortal game: " (to-string immortal))
             (println "Is black check-mate: " (is-check-mate? immortal "b"))))      
  )

)
   
