(ns dodgeball.combat
  (:require [reagent.core :refer [atom]]
            [dodgeball.state :as state]
            [dodgeball.unit :as unit]))

(defn out-of-bounds [x y]
  (or
   (< x 0)
   (> x 7)
   (< y 0)
   (> y 4)))

(defn reset-ball [id]
  (case id
    11 {:y 0 :x 4}
    12 {:y 1 :x 4}
    13 {:y 2 :x 4}
    14 {:y 3 :x 4}
    15 {:y 4 :x 4}))

(defn roll [n]
  (+ 1 (rand-int n)))

(defn deflect [x y]
  (let [magnitude (roll 2)
        dir
        (case (roll 2)
          1 (map #(* magnitude %) [ 0 -1])
          2 (map #(* magnitude %) [-1  0])
          3 (map #(* magnitude %) [ 1  0])
          4 (map #(* magnitude %) [ 0  1]))]
    (if (out-of-bounds x y)
      (reset-ball (:id (state/selected-unit)))
      (zipmap [:x :y] (map + dir [x y])))))

(defn slope [unit]
  (let [{x1 :x y1 :y} (state/selected-unit)
        {x2 :x y2 :y} unit]
    (/ (- x2 x1) (- y2 y1))))

(defn clamp-slope [m]
  (cond (> m  4)  4
        (< m -4) -4
        :else m))

(defn trajectory [unit magnitude]
  (let [m  (clamp-slope (slope unit))
        dy (/ magnitude (Math/sqrt (+ 1 (* m m))))
        dx (* m dy)]
    (println "M: " m " DX: " dx " DY: " dy)
    {:x (+ (:x (state/selected-unit)) (Math/floor dx))
     :y (+ (:y (state/selected-unit)) (Math/floor dy))}))

(defn position-ball [{:keys [x y] :as coords} id]
  (if (out-of-bounds x y)
    (reset-ball id)
    coords))

(defn miss [filtered-units updated-unit defense-unit]
  (println "DODGE!")

  (let [ball (conj (:ball (state/selected-unit))
                   (position-ball (trajectory defense-unit unit/attack-range) (:id (:ball (state/selected-unit)))))]
    (println "filtered units: " filtered-units " ball: " ball " updated unit: " updated-unit)
    (swap! state/game assoc :units (conj filtered-units ball updated-unit defense-unit))))

(defn hit [filtered-units updated-unit defense-unit]
  (println "HIT!")

  (let [ball (conj (:ball (state/selected-unit)) (deflect (:x defense-unit) (:y defense-unit)))]
    (println "filtered units: " filtered-units " ball: " ball " updated unit: " updated-unit)
    (swap! state/game assoc :units (conj filtered-units ball updated-unit))))

(defn attack [x y]
  (let [updated-unit   (conj (state/selected-unit) {:ball nil})
        attack-points  (roll 8)
        defense-points (roll 8)
        defense-unit   (unit/find-one-unit-by x y (state/defense))
        filtered-units   (filter (apply every-pred [#(not= % defense-unit) #(not= % (state/selected-unit))]) (:units @state/game))]
    (println "attack dice: " attack-points "defense dice: " defense-points)
    (if (<= attack-points defense-points)
      (miss filtered-units updated-unit defense-unit)
      (hit filtered-units updated-unit defense-unit))))