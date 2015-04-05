(ns dodgeball.combat
  (:require [reagent.core :as reagent :refer [atom]]
            [dodgeball.state :as state]
            [dodgeball.unit :as unit]))

(defn offense-team-without [u]
  (filter #(not= u %) (unit/active-team)))

(defn defense-team-without [u]
  (filter #(not= u %) (unit/all-unit-coords-for (unit/defense))))

(defn ball-deflect [x y]
  (let [magnitude (+ 1 (rand-int 3))
        dir (case
              (+ 1 (rand-int 4))

              1
              (map #(* magnitude %) [0 -1])

              2
              (map #(* magnitude %) [-1 0])

              3
              (map #(* magnitude %) [1 0])

              4
              (map #(* magnitude %) [0 1]))]
    (map + dir [x y])))

(defn miss [updated-unit offense-team defense-team]
  (swap! state/game assoc-in [:units :balls] (conj (get-in @state/game [:units :balls]) (:ball (unit/selected-unit))))
  (swap! state/game assoc-in [:units (:turn @state/game)] (conj offense-team updated-unit)))

(defn hit [updated-unit offense-team defense-team]
  (swap! state/game assoc-in [:units :balls] (conj (get-in @state/game [:units :balls]) (:ball (unit/selected-unit))))
  (swap! state/game assoc-in [:units (:turn @state/game)] (conj offense-team updated-unit))
  (swap! state/game assoc-in [:units (unit/defense)] defense-team))

(defn attack [x y]
  (let [updated-unit   (conj (unit/selected-unit) {:ball nil})
        attack-points  (+ 1 (rand-int 8))
        defense-points (+ 1 (rand-int 8))
        offense-team   (offense-team-without (unit/selected-unit))
        defense-team   (defense-team-without (unit/unit-by-type x y (unit/defense)))]
    (println "attack dice: " attack-points "defense dice: " defense-points)
    (if (<= attack-points defense-points)
      (miss updated-unit offense-team defense-team)
      (hit  updated-unit offense-team defense-team))))


    ;(println (unit/all-unit-coords-for :balls) (ball-deflect (:x defense-unit)
    ;                                                         (:y defense-unit)))
    ;(swap! state/game assoc-in [:units :balls] (conj (unit/all-unit-coords-for :balls)
    ;                                                 (ball-deflect (:x defense-unit)
    ;                                                               (:y defense-unit))))



