(ns reagent-chess-board.board
    (:require [reagent.core :as reagent] 
              [clojure.string]
              [goog.dom :as dom]
              [goog.events :as events] 
              [chess-utils.core :as chess]
              [cljs.core.async :refer [>! <! put! close! timeout chan]])
    (:require-macros [cljs.core.async.macros :refer [go]]))

(def light-sq "#f0d9b5")
(def bright-light-sq "#ffffdf")
(def dark-sq "#d18b47")
(def bright-dark-sq "#ffaf6f")

(def ^:export xor56 (partial bit-xor 56))
(def ^:export xor7 (partial bit-xor 7))
(defn ^:export fig-map [a] (apply assoc {} (flatten (filter (fn [tup] (not= \0 (second tup))) (map-indexed #(vector %1 %2) a)))))

(def ^:export figures-map {\P "pw" \p "p" \N "nw" \n "n" \B "bw" \b "b" \R "rw" \r "r" \Q "qw" \q "q" \K "kw" \k "k"}) 

(def ^:export original-figures {8 \P 9 \P 10 \P 11 \P 12 \P 13 \P 14 \P 15 \P
                       0 \R 7 \R 3 \Q 4 \K 1 \N 6 \N 2 \B 5 \B  
                       48 \p  49 \p 50 \p 51 \p 52 \p 53 \p 54 \p 55 \p 
                       56 \r 63 \r  59 \q 60 \k 57 \n 62 \n 58 \b  61 \b})


(defonce ^:export base-game (reagent/atom (chess/make-game)))

(defonce  ^:export status (reagent/atom {
                                     :figure-set "default" 
                                     :sq-size 60 
                                     :flipped? false 
                                     :can-move? true
                                     :sq-from -1
                                     :sq-to -1
                                     :crowning nil
                                     :curr-pos (dec (count (:fens @base-game)))
                                     :figures (fig-map (:arr (nth (:fens  @base-game) 0)))
                                    }))

(defn ^:export set-sq-to! [to] (swap! status #(assoc % :sq-to to)))
(defn ^:export set-sq-from! [from] (swap! status #(assoc % :sq-from from)))
(defn ^:export set-crowning! [figure] (swap! status #(assoc % :crowning figure)))
(defn ^:export get-sq-to [] (:sq-to @status))
(defn ^:export get-sq-from [] (:sq-from @status))
(defn ^:export get-crowning [] (:crowning @status))
(defn ^:export get-sq-size [] (:sq-size @status))
(defn ^:export set-sq-size! [new-size] (swap! status #(assoc % :sq-size new-size)))
(defn ^:export go-to [n]
  (let [max-num (dec (count (:moves @base-game)))
        num  (cond (neg? n) 0 (> n max-num) max-num :else n)]
   (swap! status #(assoc % :curr-pos num :figures (fig-map (:arr (nth (:fens  @base-game) num)))))))
(defn ^:export reset-figures! [] (reset! base-game (chess/make-game)) (go-to 0)) 
(defn ^:export clean-figures! [] (reset! base-game (chess/make-game 
                                                     {:fen chess/empty-fen-string :fens [(chess/make-fen chess/empty-fen-string)]})) 
                                 (go-to 0)) 
(defn ^:export enlarge-10! [] (swap! status #(assoc % :sq-size  (+ (:sq-size @status) 10))))
(defn ^:export reduce-10! [] (swap! status #(assoc % :sq-size  (- (:sq-size @status) 10))))
(defn ^:export get-figure-set [] (:figure-set @status))
(defn ^:export set-figure-set! [new-dir] (swap! status #(assoc % :figure-set new-dir)))
(defn ^:export flip! [] (swap! status #(assoc % :flipped? (not (:flipped? @status)))))
(defn ^:export toggle-can-move! [] (swap! status #(assoc % :can-move? (not (:can-move? @status)))))
(defn ^:export reset-move-vars! [] (set-crowning! nil) (set-sq-to! -1) (set-sq-from! -1)) 

(defn ^:export move [& cb]
    (when-let [san (chess/coords-to-san (last (:fens @base-game)) (:sq-from @status) (:sq-to @status) (:crowning @status))]
     (when-let [new-game (chess/move @base-game san)]
      (reset! base-game new-game)
      (go-to (dec (count (:fens @base-game))))
      (swap! status #(assoc % :sq-from -1 :sq-to -1 :crowning nil))
      (when cb (apply (first cb) (rest cb))))))


(defn ^:export do-scroll! []
  (let [elem (.getElementById js/document "moves-body")]
    (set! (.-scrollTop  elem) (.-scrollHeight elem))))

(defn ^:export get-symbol-moves []
  (clojure.string/replace (chess/game-moves-to-string @base-game)  #"([NBRQK])" #(str (chess/unicode-map (second %)))))

(defn ^:export mk-img [src]
  (let [img (.createElement js/document "img")]
    (set! (.-src img) src)
    (set! (-> img .-style .-width) (str (int (* (:sq-size @status) 1)) "px"))
    (set! (-> img .-style .-height) (str (int (* (:sq-size @status) 1)) "px"))
    (set! (-> img .-style .-background) "transparent")
    (set! (-> img .-style .-position) "absolute")
    (set! (-> img .-id) "drag-image")
    (set! (-> img .-draggable) true)
    (set! (-> img .-z-index) -1000)
    img)) 
    
(defn ^:export mk-pixel []
  (let [img (.createElement js/document "img")]
    (set! (.-src img) "img/pixel.gif")
    img)) 

(defn ^:export show-crowning-dialog [sq-color crowning-color channel]
  (let [board (js/$ "#main-board") 
        crowning-dialog (js/$ "#crowning-div")
        crowning-imgs (js/$ ".crowning-img")
        board-left (.-left (.offset board))
        board-top (.-top (.offset board))
        row (bit-xor 7 (chess/row (:sq-to @status)))
        col (chess/col (:sq-to @status))
        dialog-top (+ board-top (* row (get-sq-size)))
        dialog-left (+ board-left (* col (get-sq-size)))
        ]
    (.css board "opacity" 0.4)
    (.css crowning-dialog "background" (if (= sq-color "b") dark-sq light-sq))
    (.css crowning-dialog "left" (str dialog-left "px"))
    (.css crowning-dialog "top" (str dialog-top "px"))
    (if (= crowning-color "w")
      (do
        (.setAttribute (.getElementById js/document "q-img") "src" (str "img/sets/" (:figure-set @status) "/qw.png")) 
        (.setAttribute (.getElementById js/document "r-img") "src" (str "img/sets/" (:figure-set @status) "/rw.png")) 
        (.setAttribute (.getElementById js/document "b-img") "src" (str "img/sets/" (:figure-set @status) "/bw.png")) 
        (.setAttribute (.getElementById js/document "n-img") "src" (str "img/sets/" (:figure-set @status) "/nw.png")) 
      )
      (do 
        (.setAttribute (.getElementById js/document "q-img") "src" (str "img/sets/" (:figure-set @status) "/q.png")) 
        (.setAttribute (.getElementById js/document "r-img") "src" (str "img/sets/" (:figure-set @status) "/r.png")) 
        (.setAttribute (.getElementById js/document "b-img") "src" (str "img/sets/" (:figure-set @status) "/b.png")) 
        (.setAttribute (.getElementById js/document "n-img") "src" (str "img/sets/" (:figure-set @status) "/n.png")) 
      )) 
    (.css crowning-dialog "visibility" "visible")
    (.on crowning-imgs "click" 
      (fn [evt]
        (go
          (.off crowning-imgs "click")
          (.css board "opacity" 1) 
          (.css crowning-dialog "visibility" "hidden")
          (.log js/console (.getAttribute (-> evt .-target) "data-tag"))
          (>! channel (.getAttribute (-> evt .-target) "data-tag")))))
    ))

(defn ^:export get-crowning-figure [sq-color crowning-color]
  (let [resp-channel (chan)]
    ;(go (>! resp-channel (clojure.string/upper-case (or (js/prompt "Choose crowning figure (Q, R, B, N)" "Q")"Q"))))
    (show-crowning-dialog sq-color crowning-color resp-channel)
    resp-channel))

(defn try-move []
  (if (and 
       (not= (:sq-from @status) (:sq-to @status)) 
       (not= (:sq-to @status) -1)
       (or (and (= "w" (:side-to-move (last (:fens @base-game)))) (#{\P\N\B\R\Q\K} ((:figures @status) (get-sq-from))))
           (and (= "b" (:side-to-move (last (:fens @base-game)))) (#{\p\n\b\r\q\k} ((:figures @status) (get-sq-from)))))) 
        (let [figure ((:figures @status) (get-sq-from))
              crowning-color (if (= figure \P) "w" "b") 
              sq-color (second (first (filter #(= (first %) (get-sq-to)) chess/sq-colors)))
              row (chess/row (get-sq-to))
              post-move-fn #(do (do-scroll!) 
                             (when (chess/is-check-mate? @base-game) 
                                (.log js/console (str (last (:moves @base-game)) " checkmate. " (:result @base-game)))))]
          (if (or (and (= figure \P) (= row 7)) (and (= figure \p) (= row 0)))
            (go (set-crowning! (<! (get-crowning-figure sq-color crowning-color)))
                (move post-move-fn) 
                (reset-move-vars!))
            ((set-crowning! nil) (move post-move-fn) (reset-move-vars!))))))

(defn on-sq-click [sq-id]
  (let [figure ((:figures @status) sq-id)]
    (cond
      (not (:can-move? @status)) nil
      (= (:sq-from @status) -1) (if-not (nil? figure) (set-sq-from! sq-id))
      (= (:sq-from @status) sq-id) (set-sq-from! -1)
      :else (do (set-sq-to! sq-id) (try-move)))))

(defn ^:export san-to-symbol [san]
  (if san
    (clojure.string/replace san #"[NBRQK]" #(chess/unicode-map %))
    ""))

(defn render-figure [figure sq]
  (let [src (str "img/sets/" (:figure-set @status) "/" (figures-map figure) ".png")] 
   [:img.figure {:width  (str (int (* (:sq-size @status) 0.9)) "px") 
         :height (str (int (* (:sq-size @status) 0.9)) "px") 
         :draggable (:can-move? @status)
         :data-where sq 
         :style {:vertical-align "middle"
                 :cursor (str "url('" src "')")
                }
         :on-load (fn [evt]
           (.draggable (js/$ (-> evt .-target)) #js {:zIndex 100 :disabled (not (:can-move? @status)) :revert true :revertDuration 1 
                                                     :start (fn [evt ui] 
                                                              (set-sq-from! -1)
                                                              (on-sq-click sq)  )}))
         :src src}]))

(defn render-sq [color sq-id]
  (let [background (cond
                    (= color "w") (if (= sq-id (get-sq-from)) bright-light-sq  light-sq)
                    :else (if (= sq-id (get-sq-from)) bright-dark-sq  dark-sq))]
  [:div.square {:style {:float "left" :width (str (get-sq-size) "px") :height (str (get-sq-size)  "px") 
                 :min-width (str (get-sq-size)  "px") :text-align "center" :vertical-align "middle"
                 :max-width (str (get-sq-size)  "px") :min-height (str (get-sq-size) "px") :max-height (str (get-sq-size) "px")
                 :background background
                 :cursor (if (and (:can-move? @status) ((:figures @status) sq-id)) "pointer" "default") 
                }
         :data-figure (or ((:figures @status) sq-id) " ") 
         :data-id sq-id
         :on-click #(on-sq-click sq-id)
        } (when-let [figure ((:figures @status) sq-id)] [render-figure figure sq-id])]))

(defn render-board []
  (let [fxor (if (:flipped? @status) xor7 xor56) 
        move-pairs (partition-all 2 (rest (:moves @base-game)))]
   [:div  
   [:div#crowning-div {:style {:display "inline-block"
                               :position "absolute" :top 0 :left 0 :border "1px solid"
                               :width (str (* 4 (get-sq-size)) "px") :height (str (get-sq-size) "px")
                               :min-width (str (* 4 (get-sq-size)) "px") :max-width (str (* 4 (get-sq-size)) "px")
                               :min-height (str (get-sq-size) "px") :max-height (str (get-sq-size) "px")
                               :z-index 5000 :background dark-sq
                               :visibility "hidden" 
                              }
                       :title "Click on the desired crowning figure"}
     [:div {:style {:width (str (get-sq-size) "px")
                    :height (str (get-sq-size) "px")
                    :display "inline-block"}}
       [:img#q-img 
         {:data-tag "Q"
          :class "crowning-img" 
          :width (str (* 0.9 (get-sq-size))) 
          :height (str (* 0.9 (get-sq-size))) 
          :src (str "img/sets/" (:figure-set @status) "/qw.png")}]
     ]
     [:div {:style {:width (str (get-sq-size) "px")
                    :height (str (get-sq-size) "px")
                    :display "inline-block"}}
       [:img#r-img 
         {:data-tag "R"
          :class "crowning-img" 
          :width (str (* 0.9 (get-sq-size))) 
          :height (str (* 0.9 (get-sq-size))) 
          :src (str "img/sets/" (:figure-set @status) "/rw.png")}]
     ]
     [:div {:style {:width (str (get-sq-size) "px")
                    :height (str (get-sq-size) "px")
                    :display "inline-block"}}
       [:img#b-img 
         {:data-tag "B"
          :class "crowning-img" 
          :width (str (* 0.9 (get-sq-size))) 
          :height (str (* 0.9 (get-sq-size))) 
          :src (str "img/sets/" (:figure-set @status) "/bw.png")}]
     ]
     [:div {:style {:width (str (get-sq-size) "px")
                    :height (str (get-sq-size) "px")
                    :display "inline-block"}}
       [:img#n-img 
         {:data-tag "N"
          :class "crowning-img" 
          :width (str (* 0.9 (get-sq-size))) 
          :height (str (* 0.9 (get-sq-size))) 
          :src (str "img/sets/" (:figure-set @status) "/nw.png")}]
     ]
   ]
   
   [:div#main-board {:style {:display "inline-block" :width (str (* 8 (get-sq-size) ) "px") :height (str (* 8 (get-sq-size) ) "px") 
                  :min-width (str (* 8 (get-sq-size) ) "px") 
                  :max-width (str (* 8 (get-sq-size) ) "px") :min-height (str (* 8 (get-sq-size) ) "px") 
                  :max-height (str (* 8 (get-sq-size) ) "px") :border "1px solid"}}
    (for [sq (map fxor (range 64))] ^{:key sq} [render-sq (second (first (filter #(= (first %) sq) chess/sq-colors))) sq])
   ]
  
  [:div {:style {:display "inline-block" :margin "1em" :padding "1em"}}
    [:div {:style {:font-size "small"}}
      [:div.table-header
        [:div [:span.numeral [:strong " "]] [:span.left-panel [:strong "Event"]] [:span.right-panel (:event @base-game)]]
        [:div [:span.numeral [:strong " "]] [:span.left-panel [:strong "Site"]] [:span.right-panel (:site @base-game)]]
        [:div [:span.numeral [:strong " "]] [:span.left-panel [:strong "Date"]] [:span.right-panel (:date @base-game)]]
        [:div {:style {:border-top "1px solid #fff"}} 
          [:span.numeral [:strong " "]] [:span.left-panel [:strong (:white @base-game)]] [:span.right-panel [:strong (:black @base-game)]]]
      ]
      [:div.table-body {:id "moves-body" :style {:height (* 6 (get-sq-size)) :min-height (* 6 (get-sq-size)) 
                  :max-height (* 6 (get-sq-size)) :overflow "auto"}}
       (let [dstatus @status]    
        (for [r (range (count move-pairs))] 
          (let [n1 (* 2 r) n2 (inc n1)] 
            ^{:key (inc r)} [:div {:style {}} 
                         [:span.numeral (inc r)] 
                         [:span.left-panel {:style 
                                {:cursor "pointer"
                                 :background (if (= (inc n1) (:curr-pos dstatus)) "#ddd" "inherit")}
                              :on-click #(go-to (inc n1))} 
                              (san-to-symbol (first (nth move-pairs r)))] 
                         [:span.right-panel {:style 
                                {:cursor "pointer"
                                 :background (if (= (inc n2) (:curr-pos dstatus)) "#ddd" "inherit")}
                               :on-click #(go-to (inc n2))} 
                              (san-to-symbol (second (nth move-pairs r)))]]))) 
      ]
      [:div.table-footer
        [:div
          [:span.numeral] [:span.left-panel] [:span.right-panel [:strong (:result @base-game)]]
        ]
      ]
    ]
  ]
  ]))

(defn board-did-mount []                    
   (js/$ (fn []
           (.droppable (js/$ "div.square") #js {:drop 
              (fn [evt ui] 
                  (let [sq-id (int (.attr (js/$ (-> evt .-target)) "data-id"))]
                    (set-sq-to! sq-id) (try-move)))}))))
  
(defn show-board []
  (reagent/create-class {:display-name "Reagent Chess Board" :reagent-render render-board :component-did-mount board-did-mount}))  
                         
