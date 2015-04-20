(ns dodgeball.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :refer [put! chan <!]]
            [dodgeball.unit :as unit]
            [dodgeball.state :as state]
            [dodgeball.actions :as actions]))

(def board-width (range 0 9))
(def board-height (range 0 5))


(defn bench []
  [:table {:class "bench"}
   (for [_ (range 0 4)]
     [:tr [:td]])])

(defn game-board [{:keys [:actions]}]
  [:div {:class "game clear"}
   [:h1 (str (:turn @state/game))]
   [:h1 (:actions @state/game)]
   [bench]
   [:table
    (doall (for [y board-height]
             [:tr {:key   y}
              (doall (for [x board-width]
                       [:td {:key      (str x y)
                             :class    (unit/css-class x y)
                             :on-click #(put! actions (actions/determine-action x y))}]))]))]
   [bench]])


(defn draw-board []
  (let [ch (chan)]
    (reagent/render
      [game-board {:actions ch}]
      (.getElementById js/document "app"))
    (actions/handle-event ch)))

(defn init! []
  (draw-board))