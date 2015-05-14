(ns dodgeball.actions
  (:require [reagent.core :refer [atom]]
            [cljs.core.async :refer [<!]]
            [clojure.set :as s]
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

; blue unit:
; 0 < x < 4
; 0 < y < 4
; not= ball
; not= team
; not= enemy
; not= self

; red unit:
; 4 < x < 8
; 0 < y < 4
; not= ball
; not= team
; not= enemy
; not= self

(defn tiles-in [x-range y-range]
  (let [[xs ys] (map identity [x-range y-range])]
    (for [x xs y ys] (vector x y))))


(def red-tiles (set (tiles-in (range 4 9) (range 0 5))))
(def blue-tiles (set (tiles-in (range 0 5) (range 0 5))))

(defn unit-tiles []
  (set (map (fn [unit] [(:x unit) (:y unit)]) (:units @state/game))))

(defn move-range [unit]
    (concat
      (tiles-in (range 4 (- (:x unit) 3)) (range 0 dodgeball.core/board-height))
      (tiles-in (range 4 dodgeball.core/board-width) (range (+ (:y unit) 3) dodgeball.core/board-height))))

(defn movable-tiles [unit]
  (println (move-range (:selected-unit @state/game)))
  (s/difference
    red-tiles
    (unit-tiles)
    (select-keys (:selected-unit @state/game) [:x :y])
    (move-range (:selected-unit @state/game))))

(defn highlight-movement-range [unit]
  (mapv
    (fn [[x y]]
      (dodgeball.core/highlight-tile "#ddd" x y))
    (movable-tiles unit)))

(defn move-unit [selected x y]
  (update-unit selected {:x x :y y})
  (deselect-unit)
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

    ; no current selection
    ; unit in question is on current team
    (cond
      (and
        (= nil selected)
        (unit/unit? x y game-state))
      (do
        (select-unit (unit/find-one-unit-by x y (:turn game-state) game-state))
        (dodgeball.core/draw-screen)
        (highlight-movement-range (:selected-unit @state/game)))

      ; a unit is selected
      ; unit in question is on defense
      (and
        (boolean selected)
        (boolean defense-unit))
      (do
        (combat/attack selected defense-unit)
        (increment-actions)
        (deselect-unit)
        (dodgeball.core/draw-screen))

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
      (do
        (deselect-unit)
        (dodgeball.core/draw-screen))

        (and
        (boolean (unit/find-one-unit-by x y :ball game-state))
        (boolean selected))
      (pickup-ball selected (unit/find-one-unit-by x y :ball game-state))

      (unit/in-movement-range? x y selected)
      (move-unit selected x y)

      :else
      (println x y (:turn game-state)))
    (if (= (:actions @state/game) 2) (switch-turns))
    ))