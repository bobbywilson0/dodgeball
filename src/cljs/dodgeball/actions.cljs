(ns dodgeball.actions
  (:require [reagent.core :refer [atom]]
            [cljs.core.async :refer [<!]]
            [dodgeball.unit :as unit]
            [dodgeball.state :as state]
            [dodgeball.combat :as combat])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn switch-turns []
  (if (= (:turn @state/game) :red)
      (swap! state/game assoc :turn :blue :actions 0)
      (swap! state/game assoc :turn :red :actions 0)))

(defn handle-event [ch]
  (go
     (loop []
       (let [event (<! ch)
             {:keys [x y]} event]
         (println event)
          (case (:type event)
            :select-unit   (unit/select-unit (:id event))
            :deselect-unit (unit/deselect-unit)
            :move-unit     (do
                             (unit/move-unit x y)
                             (swap! state/game update-in [:actions] inc)
                             (unit/deselect-unit))
            :pickup-ball   (do
                             (unit/pickup-ball x y)
                             (swap! state/game update-in [:actions] inc)
                             (unit/deselect-unit))
            :attack        (do
                             (combat/attack x y)
                             (swap! state/game update-in [:actions] inc)
                             (unit/deselect-unit)))
         (if (= (:actions @state/game) 2) (switch-turns))
         (recur)))))

(defn determine-action [x y]
  (let [unit (unit/find-one-unit-by x y (:turn @state/game))]
    (println (unit/in-movement-range? x y (state/selected-unit)))

    (cond
     (and
      (= nil (state/selected-unit))
      (boolean unit))
     {:type   :select-unit
      :id     (:id unit)
      :x      x
      :y      y}

     (and
       (boolean (state/selected-unit))
       (boolean (unit/find-one-unit-by x y (state/defense))))
     {:type :attack
      :id   (:id unit)
      :x    x
      :y    y}
     (or
       (boolean unit)
       (not (unit/in-movement-range? x y (state/selected-unit)))
       (and
         (= nil (state/selected-unit))
         (boolean (unit/find-one-unit-by x y (state/defense)))))
     {:type :deselect-unit
      :id   (:id unit)
      :x    x
      :y    y}

     (and
       (boolean (unit/find-one-unit-by x y :ball))
       (boolean (state/selected-unit)))
     {:type :pickup-ball
      :id   (:id (unit/find-one-unit-by x y :ball))
      :x    x
      :y    y}


     (unit/in-movement-range? x y (state/selected-unit))
     {:type :move-unit
      :id   (:id (state/selected-unit))
      :x    x
      :y    y}

     :else
       (println x y (:turn @state/game)))))