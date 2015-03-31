(ns dodgeball.core
  (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [cljs.core.async :refer [put! chan <!]]
              [cljsjs.react :as react]
              [dodgeball.unit :as unit]
              [dodgeball.state :as state])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History))


(def board-length (range 0 11))

(defn border-top [y]
  (cond
    (or (= y 4) (= y 7)) "middle-top"))

(defn switch-turns []
  (if (= (:turn @state/game) :red-team)
    (do
      (swap! state/game assoc :turn :blue-team)
      (swap! state/game assoc :actions 0))
    (do
      (swap! state/game assoc :turn :red-team)
      (swap! state/game assoc :actions 0))))

(defn determine-action [x y]
  (println (unit/selected-unit))
  (cond
   (and
    (= nil (unit/selected-unit))
    (boolean (unit/unit? x y (:turn @state/game))))
   {:type   :select-unit
    :id     (:id (unit/unit? x y (:turn @state/game)))
    :coords {:x x :y y}}

   (or
     (boolean (unit/unit? x y (:turn @state/game)))
     (not (unit/in-range? x y (:id (unit/selected-unit)))))
   {:type   :deselect-unit
    :id     (:id (unit/unit? x y (:turn @state/game)))
    :coords {:x x :y y}}

   (boolean (unit/unit? x y :balls))
   {:type   :pickup-ball
    :id     (:id (unit/unit? x y :balls))
    :coords {:x x :y y}}

   (boolean (unit/unit? x y (unit/defense)))
   {:type :attack
    :id (:id (unit/unit? x y (:turn @state/game)))
    :coords {:x x :y y}}


   (unit/in-range? x y (:id (unit/unit? x y (:turn @state/game))))
   {:type :move-unit
    :id (:id (unit/unit? x y (:turn @state/game)))
    :coords {:x x :y y}}

   :else
     (println x y (:turn @state/game))))

(defn game-board [{:keys [:actions]}]
  [:div
    [:h1 (str (:turn @state/game))]
    [:h1 (:actions @state/game)]
    [:table
     (doall (for [y board-length]
       [:tr {:key y
             :class (border-top y)}
        (doall (for [x board-length]
           [:td {:key (str x y)
                 :class (unit/unit-class x y)
                 :on-click #(put! actions (determine-action x y))}]))]))]])

(defn mount-root []
  (let [ch (chan)]
  (reagent/render
   [game-board {:actions ch}]
   (.getElementById js/document "app"))
  (go
   (loop []
     (let [event (<! ch)
           x (:x (:coords event))
           y (:y (:coords event))]
       (println event)
        (case (:type event)
          :select-unit   (unit/select-unit (:id event))
          :deselect-unit (unit/deselect-unit)
          :move-unit     (do
                           (unit/move-unit x y)
                           (swap! state/game update-in [:actions] inc)
                           (unit/deselect-unit)
                           (if (= (:actions @state/game) 2) (switch-turns)))
          :pickup-ball   (do
                           (unit/pickup-ball x y)
                           (swap! state/game update-in [:actions] inc)
                           (unit/deselect-unit)
                           (if (= (:actions @state/game) 2) (switch-turns)))
          :attack        (do
                           (unit/attack x y)
                           (swap! state/game update-in [:actions] inc)
                           (unit/deselect-unit)
                           (if (= (:actions @state/game) 2) (switch-turns))))
       (recur))))))

(defn init! []
  (mount-root))
