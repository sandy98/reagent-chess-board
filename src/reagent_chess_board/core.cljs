(ns reagent-chess-board.core
    (:require [reagent.core :as reagent]
              [reagent-chess-board.board :as board]))

(defn show-buttons []
  [:div
    [:button {:on-click #(board/go-to 0)} " << "]
    [:button {:on-click #(board/go-to (dec (:curr-pos @board/status)))} " < "]
    [:button {:on-click #(board/go-to (inc (:curr-pos @board/status)))} " > "]
    [:button {:on-click #(board/go-to (dec (count (:moves @board/base-game))))} " >> "]
  ])    
                         
(defn ^:export main []
  (reagent/render [board/show-board]
                  (.getElementById js/document "board-1"))
  (reagent/render [show-buttons]
                  (.getElementById js/document "buttons-div")))

