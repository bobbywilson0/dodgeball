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

(defn update-unit [unit & to-conj]
  (let [updated-unit   (apply conj unit to-conj)
        filtered-units (filter #(not= unit %) (:units @state/game))]
    (swap! state/game assoc :units (conj filtered-units updated-unit))))

(defn select-unit [{:keys [x y]}]
  (update-unit (unit/find-one-unit-by x y (:turn @state/game) @state/game) {:selected true}))

(defn deselect-unit [unit]
  (update-unit unit {:selected false}))

(defn increment-actions []
  (swap! state/game update :actions inc))

(defn handle-event [ch]
  (go
    (loop []
      (let [event (<! ch)
            {:keys [selected target]} event]
        (println event)

        (case (:type event)
          :select-unit   (select-unit target)
          :deselect-unit (deselect-unit selected)
          :move-unit     (do
                           (update-unit selected target {:selected false})
                           (increment-actions))
          :pickup-ball   (do
                           (unit/pickup-ball selected target)
                           (increment-actions))
          :attack        (do
                           (combat/attack selected target)
                           (increment-actions)
                           (deselect-unit (state/selected-unit))))
        (if (= (:actions @state/game) 2) (switch-turns))
        (recur)))))

(defn determine-action [x y]
  (let [selected     (state/selected-unit)
        game-state   @state/game
        defense-unit (unit/find-one-unit-by x y (state/defense) game-state)]

    ; no current selection
    ; unit in question is on current team
    (cond
      (and
        (= nil selected)
        (boolean (unit/find-one-unit-by x y (:turn game-state) game-state)))
      {:type     :select-unit
       :target   {:x x :y y}
       :selected selected}

      ; a unit is selected
      ; unit in question is on defense
      (and
        (boolean selected)
        (boolean defense-unit))
      {:type     :attack
       :target   defense-unit
       :selected selected}

      ; selected unit is not in movement range
      ; or unit in question is selected unit
      ; or
      (or
        (not (unit/in-movement-range? x y selected))
        (and
          (= x (:x selected))
          (= y (:y selected)))
        (and
          (= nil selected)
          (boolean defense-unit)))
      {:type     :deselect-unit
       :target   {:x x :y y}
       :selected selected}

      (and
        (boolean (unit/find-one-unit-by x y :ball game-state))
        (boolean selected))
      {:type     :pickup-ball
       :target   (unit/find-one-unit-by x y :ball game-state)
       :selected selected}

      (unit/in-movement-range? x y selected)
      {:type     :move-unit
       :target   {:x x :y y}
       :selected selected}

      :else
      (println x y (:turn game-state)))))