(ns dodgeball.unit
  (:require [reagent.core :refer [atom]]
            [dodgeball.state :as state]))

(def move-range 4)
(def attack-range 4)

(defn distance [a b]
  (Math/abs (- b a)))

(defn find-one-unit-by [x y unit-type]
  (first
    (filter
      (fn [unit]
        (and
          (= x (:x unit))
          (= y (:y unit))
          (= unit-type (:type unit))))
      (:units @state/game))))

(defn update-unit [unit to-conj]
  (let [updated-unit   (conj unit to-conj)
        filtered-units (filter #(not= unit %) (:units @state/game))]
    (swap! state/game assoc :units (conj filtered-units updated-unit))))

(defn select-unit [target]
  (update-unit (find-one-unit-by (:x target) (:y target) (:turn @state/game)) {:selected true}))

(defn deselect-unit [unit]
  (update-unit unit {:selected false}))

(defn move-unit [selected {:keys [x y] :as target}]
  (println selected)
  (update-unit selected target))

(defn unit-adjacent-to-ball? [ball unit]
  (or
    (and
      (= (:x unit) (dec (:x ball)))
      (= (:y unit) (:y ball)))
    (and
      (= (:x unit) (inc (:x ball)))
      (= (:y unit) (:y ball)))
    (and
      (= (:y unit) (dec (:y ball)))
      (= (:x unit) (:x ball)))
    (and
      (= (:y unit) (inc (:y ball)))
      (= (:x unit) (:x ball)))))

(defn pickup-ball [selected-unit unit]
  (let [updated-unit   (conj selected-unit {:ball unit})
        filtered-units (filter
                         (apply every-pred [#(not= unit %) #(not= selected-unit %)])
                         (:units @state/game))]
    (if (unit-adjacent-to-ball? unit selected-unit)
      (swap! state/game assoc :units (conj filtered-units updated-unit)))))

(defn selected? [x y]
  (let [unit (state/selected-unit)]
    (and
      (= x (:x unit))
      (= y (:y unit)))))

(defn css-class [x y]
  (let [blue-unit (find-one-unit-by x y :blue)
        red-unit  (find-one-unit-by x y :red)
        ball-unit (find-one-unit-by x y :ball)]
    (str
      (cond
        (and
          (boolean blue-unit)
          (:ball blue-unit))
        "blue-team-ball"

        (boolean blue-unit)
        "blue-team"

        (and
          (boolean red-unit)
          (:ball red-unit))
        "red-team-ball"

        (boolean red-unit)
        "red-team"

        (boolean ball-unit)
        "ball")

      (cond
        (selected? x y)
        (str " " "selected")))))

(defn in-movement-range? [x2 y2 selected-unit]
  (let [{:keys [:x :y]} selected-unit]
    (and
      (or
        (and
          (= :blue (:turn @state/game))
          (<= x2 4))
        (and
          (= :red (:turn @state/game))
          (>= x2 4)))
      (<=
        (reduce
          +
          (map distance [x2 y2] [x y]))
        move-range))))