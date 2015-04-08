(ns dodgeball.core
  (:require [reagent.core :as reagent :refer [atom]]
              [cljs.core.async :refer [put! chan <!]]
              [dodgeball.unit :as unit]
              [dodgeball.state :as state]
              [dodgeball.actions :as actions]))

(def board-length (range 0 11))

(defn border-top [y]
  (cond
    (or (= y 4) (= y 7)) "middle-top"))
(defn bench []
  [:table
   [:tr (for [n (range 0 4)]
          [:td])]])
(defn game-board [{:keys [:actions]}]
  [:div
    [:h1 (str (:turn @state/game))]
    [:h1 (:actions @state/game)]
    [bench]
    [:table
     (doall (for [y board-length]
       [:tr {:key y
             :class (border-top y)}
        (doall (for [x board-length]
           [:td {:key (str x y)
                 :class (unit/unit-class x y)
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
