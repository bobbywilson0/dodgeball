(ns dodgeball.combat
  (:require [reagent.core :as reagent :refer [atom]]
            [dodgeball.state :as state]
            [dodgeball.unit :as unit]))

(defn offense-team-without [u]
  (filter #(not= u %) (unit/active-team)))

(defn defense-team-without [u]
  (filter #(not= u %) (unit/all-unit-coords-for (unit/defense))))

(defn out-of-bounds [x y]
  (or
   (< x 0)
   (> x 10)
   (< y 0)
   (> y 10)))

(defn reset-ball [id]
  (case id
     1 {:x 1 :y 5}
     2 {:x 3 :y 5}
     3 {:x 5 :y 5}
     4 {:x 7 :y 5}
     5 {:x 9 :y 5}))

(defn deflect [x y]
  (let [magnitude (+ 1 (rand-int 3))
        dir
        (case (+ 1 (rand-int 4))
          1 (map #(* magnitude %) [ 0 -1])
          2 (map #(* magnitude %) [-1  0])
          3 (map #(* magnitude %) [ 1  0])
          4 (map #(* magnitude %) [ 0  1]))]
    (if (out-of-bounds x y)
      (reset-ball (get-in (unit/selected-unit) [:ball :id]))
      (zipmap [:x :y] (map + dir [x y])))))

(defn slope [unit]
  (map - (vals (:coords unit)) (vals (:coords (unit/selected-unit)))))

(defn miss [updated-unit offense-team defense-team defense-unit]
  (println (slope defense-unit))
  (swap! state/game assoc-in [:units :balls] (conj (get-in @state/game [:units :balls]) (:ball (unit/selected-unit))))
  (swap! state/game assoc-in [:units (:turn @state/game)] (conj offense-team updated-unit)))

(defn hit [updated-unit offense-team defense-team defense-unit]
  (let [coords (:coords defense-unit)
        ball   (assoc (:ball (unit/selected-unit)) :coords (deflect (:x coords) (:y coords)))]
    (swap! state/game assoc-in [:units :balls] (conj (get-in @state/game [:units :balls]) ball))
    (swap! state/game assoc-in [:units (:turn @state/game)] (conj offense-team updated-unit))
    (swap! state/game assoc-in [:units (unit/defense)] defense-team)))

(defn attack [x y]
  (let [updated-unit   (conj (unit/selected-unit) {:ball nil})
        attack-points  (+ 1 (rand-int 8))
        defense-points (+ 1 (rand-int 8))
        offense-team   (offense-team-without (unit/selected-unit))
        defense-unit   (unit/unit-by-type x y (unit/defense))
        defense-team   (defense-team-without defense-unit)]
    (println "attack dice: " attack-points "defense dice: " defense-points)
    (if (<= attack-points defense-points)
      (miss updated-unit offense-team defense-team defense-unit)
      (hit  updated-unit offense-team defense-team defense-unit))))


