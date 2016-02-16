(ns reagent-chess-board.core
    (:require [reagent.core :as reagent]
              [reagent-chess-board.board :as board]))
                         
(defn ^:export main []
  (reagent/render [board/show-board]
                  (.getElementById js/document "app")))

