(ns dodgeball.actions
  (:require [reagent.core :refer [atom]]
            [cljs.core.async :refer [<!]]
            [clojure.set :as s]
            [cljs.core.match :refer-macros [match]]
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

(defn select-unit [unit]
  (swap! state/game assoc :selected-unit unit))

(defn deselect-unit []
  (swap! state/game assoc :selected-unit nil))

(defn increment-actions []
  (swap! state/game update :actions inc))

(defn tiles-in [x-range y-range]
  (let [[xs ys] (map identity [x-range y-range])]
    (for [x xs y ys] (vector x y))))


(def red-tiles  (set (tiles-in (range 4 9) (range 0 5))))
(def blue-tiles (set (tiles-in (range 0 5) (range 0 5))))

(defn team-tiles [unit]
  (println unit)
  (case (:type unit)
    :red  red-tiles
    :blue blue-tiles
    nil ""))

(defn unit-tiles []
  (set (map (fn [unit] [(:x unit) (:y unit)]) (:units @state/game))))

(defn movable-tiles [unit]
  (s/difference
    (team-tiles unit)
    (unit-tiles)
    (select-keys (:selected-unit @state/game) [:x :y])))

(defn select-movement-range [unit]
  (swap! state/game assoc :movement-range (movable-tiles unit)))

(defn deleselect-movement-range []
  (swap! state/game assoc :movement-range nil))

(defn move-unit [selected x y]
  (update-unit selected {:x x :y y})
  (deselect-unit)
  (deleselect-movement-range)
  (increment-actions))

(defn pickup-ball [selected-unit unit]
  (let [updated-unit   (conj selected-unit {:ball unit})
        filtered-units (filter
                         (apply every-pred [#(not= unit %) #(not= selected-unit %)])
                         (:units @state/game))]
    (if (unit/unit-adjacent-to-ball? unit selected-unit)
      (swap! state/game assoc :units (conj filtered-units updated-unit))))
  (deselect-unit)
  (increment-actions))

(defn determine-action [[x y]]
  (let [selected     (:selected-unit @state/game)
        game-state   @state/game
        defense-unit (unit/find-one-unit-by x y (state/defense) game-state)]

    (match [(not= nil selected)
            (boolean (unit/unit? x y game-state))
            (boolean defense-unit)
            (unit/in-movement-range? x y selected)]

           ; select unit
           [false true false _]
           (do
             (select-unit (unit/find-one-unit-by x y (:turn game-state) game-state))
             (select-movement-range (:selected-unit @state/game))
             (dodgeball.core/draw-screen))

           ; attack unit
           [true _ true _]
           (do
             (combat/attack selected defense-unit)
             (increment-actions)
             (deselect-unit)
             )

           ; move unit
           [true false false true]
           (do
             (move-unit selected x y)
             (deselect-unit)
             (deleselect-movement-range)
             (dodgeball.core/draw-screen))

           ;; deselect unit
           [true true _ _]
           (do
             (deselect-unit)
             (deleselect-movement-range)
             (dodgeball.core/draw-screen))

           ; deselect
           [true _ _ false]
           (do
             (deselect-unit)
             (deleselect-movement-range)
             (dodgeball.core/draw-screen))

           [false _ _ false]
           (do
             (deselect-unit)
             (deleselect-movement-range)
             (dodgeball.core/draw-screen))

           ; move unit
           [true false false true]
           (do
             (move-unit selected x y)
             (deselect-unit)
             (deleselect-movement-range)
             (dodgeball.core/draw-screen)))

    (if (= (:actions @state/game) 2) (switch-turns))))