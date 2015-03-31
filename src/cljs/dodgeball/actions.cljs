(ns dodgeball.actions
  (:require [reagent.core :as reagent :refer [atom]]
            [dodgeball.unit :as unit]
            [dodgeball.state :as state])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn switch-turns []
  (if (= (:turn @state/game) :red-team)
    (do
      (swap! state/game assoc :turn :blue-team)
      (swap! state/game assoc :actions 0))
    (do
      (swap! state/game assoc :turn :red-team)
      (swap! state/game assoc :actions 0))))

(defn handle-event [ch]
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
         (recur)))))

(defn determine-action [x y]
  (println (unit/selected-unit))
  (let [team-unit (unit/unit? x y (:turn @state/game))]
    (cond
     (and
      (= nil (unit/selected-unit))
      (boolean team-unit))
     {:type   :select-unit
      :id     (:id team-unit)
      :coords {:x x :y y}}

     (or
       (boolean team-unit)
       (not (unit/in-range? x y (:id (unit/selected-unit)))))
     {:type   :deselect-unit
      :id     (:id team-unit)
      :coords {:x x :y y}}

     (boolean (unit/unit? x y :balls))
     {:type   :pickup-ball
      :id     (:id (unit/unit? x y :balls))
      :coords {:x x :y y}}

     (boolean (unit/unit? x y (unit/defense)))
     {:type :attack
      :id (:id team-unit)
      :coords {:x x :y y}}


     (unit/in-range? x y (:id team-unit))
     {:type :move-unit
      :id (:id team-unit)
      :coords {:x x :y y}}

     :else
       (println x y (:turn @state/game)))))
