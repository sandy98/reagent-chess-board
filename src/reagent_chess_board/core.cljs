(ns reagent-chess-board.core
    (:require [reagent.core :as reagent]
              [reagent-chess-board.board :as board]))

(defn show-buttons []
  [:div
    [:button {:on-click #(board/go-to 0)} " << "]
    [:button {:on-click #(board/go-to (dec (:curr-pos @board/status)))} " < "]
    [:button {:on-click #(board/go-to (inc (:curr-pos @board/status)))} " > "]
    [:button {:on-click #(board/go-to (dec (count (:moves @board/base-game))))} " >> "]
    [:span "     -     "]
    [:button {:on-click board/enlarge-10!} "+ 10"]
    [:button {:on-click board/reduce-10!} "- 10"]
    [:button {:on-click board/flip!} (if (:flipped? @board/status) "Unflip board" "Flip board")]
    [:button {:on-click board/clean-figures!} "Clear board"]
    [:button {:on-click board/reset-figures!} "Restore board"]
    [:button {:on-click board/toggle-can-move!} (if (:can-move? @board/status) "Prevent moving" "Allow moving")]
    [:label {:for "cbo-sets"} "Chess set:   "][:select {:id "cbo-sets" :on-change #(board/set-figure-set! (-> % .-target .-value))}
     [:option {:value "default"} "Default"]
     [:option {:value "eyes"} "Eyes"]
     [:option {:value "modern"} "Modern"]
     [:option {:value "fantasy"} "Fantasy"]
     [:option {:value "spatial"} "Spatial"]
     [:option {:value "veronika"} "Veronika"]
     [:option {:value "alt1"} "Alt 1"]
    ]
  ])    
                         
(defn ^:export main []
  (reagent/render [board/show-board]
                  (.getElementById js/document "board-1"))
  (reagent/render [show-buttons]
                  (.getElementById js/document "buttons-div")))

