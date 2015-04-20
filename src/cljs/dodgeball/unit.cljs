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

(defn find-unit-by-id [id]
  (first
    (filter
      #(= id (:id %))
      (:units @state/game))))

(defn select-unit [id]
  (let [updated-unit   (conj
                         (find-unit-by-id id)
                         {:selected true})
        filtered-units (filter #(not= id (:id %)) (:units @state/game))]
    (swap! state/game assoc :units (conj filtered-units updated-unit))))

(defn deselect-unit []
  (let [selected-unit (state/selected-unit)
        updated-unit  (conj
                        selected-unit
                        {:selected false})
        filtered-units (filter #(not= selected-unit %) (:units @state/game))]
    (swap! state/game assoc :units (conj filtered-units updated-unit))))

(defn move-unit [x y]
  (let [selected-unit (state/selected-unit)
        updated-unit  (conj
                        selected-unit
                        {:x x :y y})
        filtered-units (filter #(not= selected-unit %) (:units @state/game))]
    (swap! state/game assoc :units (conj filtered-units updated-unit))))

(defn unit-adjacent-to-ball? [ball unit]
  (let [ball-x (:x ball)
        ball-y (:y ball)
        unit-y (:y unit)
        unit-x (:x unit)]
    (and
      (or
        (= unit-x (dec ball-x))
        (= unit-x (inc ball-x))
        (= unit-y (dec ball-y))
        (= unit-y (inc ball-y))))))

(defn pickup-ball [x y]
  (let [selected-unit (state/selected-unit)
        ball          (find-one-unit-by x y :ball)
        updated-unit  (conj selected-unit {:ball ball})
        filtered-units (filter
                         (apply every-pred [#(not= ball %) #(not= selected-unit %)])
                         (:units @state/game))]
    (if (unit-adjacent-to-ball? ball selected-unit)
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