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
            {:keys [selected target]} event]
        (println "SELECTED" selected)
        (case (:type event)
          :select-unit (unit/select-unit target)
          :deselect-unit (unit/deselect-unit selected)
          :move-unit (do
                       (unit/move-unit selected target)
                       (swap! state/game update-in [:actions] inc)
                       (unit/deselect-unit (state/selected-unit)))
          :pickup-ball (do
                         (unit/pickup-ball selected target)
                         (swap! state/game update-in [:actions] inc)
                         (unit/deselect-unit (state/selected-unit)))
          :attack (do
                    (combat/attack selected target)
                    (swap! state/game update-in [:actions] inc)
                    (unit/deselect-unit selected)))
        (if (= (:actions @state/game) 2) (switch-turns))
        (recur)))))

(defn determine-action [x y]
  (let [selected (state/selected-unit)
        defense-unit (unit/find-one-unit-by x y (state/defense))]

    (cond
      (and
        (= nil selected)
        (boolean (unit/find-one-unit-by x y (:turn @state/game))))
      {:type :select-unit
       :target {:x x :y y}
       :selected selected}

      (and
        (boolean selected)
        (boolean defense-unit))
      {:type :attack
       :target defense-unit
       :selected selected}

      (or
        (not (unit/in-movement-range? x y selected))
        (and
          (= nil selected)
          (boolean defense-unit)))
      {:type :deselect-unit
       :target {:x x :y y}
       :selected selected}

      (and
        (boolean (unit/find-one-unit-by x y :ball))
        (boolean selected))
      {:type :pickup-ball
       :target (unit/find-one-unit-by x y :ball)
       :selected selected}

      (unit/in-movement-range? x y selected)
      {:type :move-unit
       :target {:x x :y y}
       :selected selected}

      :else
      (println x y (:turn @state/game)))))