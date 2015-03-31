(ns dodgeball.unit
  (:require [reagent.core :as reagent :refer [atom]]
            [dodgeball.state :as state]))

(def move-range 6)

(defn active-team []
  (let [game @state/game]
  ((:turn game) (:units game))))

(defn defense []
  (if (= (:turn @state/game) :red-team)
    :blue-team
    :red-team))

(defn distance [a b]
  (Math/abs (- b a)))

(defn all-unit-coords-for [unit-type]
  (get-in @state/game [:units unit-type]))

(defn unit? [x y unit-type]
  (first
   (filter #(= {:x x :y y} (:coords %)) (all-unit-coords-for unit-type))))

(defn selected-unit []
  (first
   (filter :selected (active-team))))

(defn active-team-unit-by-id [id]
  (first
   (filter #(= id (:id %))
           (active-team))))

(defn select-unit [id]
  (let [updated-unit
        (conj
         (active-team-unit-by-id id)
         {:selected true})
        team
        (filter #(not= id (:id %)) (active-team))]
    (swap! state/game assoc-in [:units (:turn @state/game)] (conj team updated-unit))))

(defn deselect-unit []
  (let [selected-unit (selected-unit)
        updated-unit
        (conj
         selected-unit
         {:selected false})
        team
        (filter #(not= selected-unit %) (active-team))]
    (swap! state/game assoc-in [:units (:turn @state/game)] (conj team updated-unit))))

(defn move-unit [x y]
  (let [selected-unit (selected-unit)
      updated-unit
      (conj
       selected-unit
       {:coords {:x x :y y}})
      team
      (filter #(not= selected-unit %) (active-team))]
    (swap! state/game assoc-in [:units (:turn @state/game)] (conj team updated-unit))))

(defn attack [x y]
  (let [selected-unit (selected-unit)
        updated-unit  (conj selected-unit {:ball false})
        defense-unit  (unit? x y (defense))
        team          (filter #(not= selected-unit %) (active-team))]
    (do
      (swap! state/game assoc-in [:units (:turn @state/game)] (conj team updated-unit))
      (println "Attack: " (rand-int 8) "Defense: " (rand-int 8)))))

(defn ball-in-front-of-unit? [ball unit]
  (let [ball-x (:x (:coords ball))
        ball-y (:y (:coords ball))
        unit-y (:y (:coords unit))
        unit-x (:x (:coords unit))]
    (and
     (= ball-x unit-x)
     (or
      (and
       (= (:turn @state/game) :blue-team)
       (= unit-y (dec ball-y)))
      (and
       (= (:turn @state/game) :red-team)
       (= unit-y (inc ball-y)))))))

(defn balls-without [x y]
  (filter #(not= (:coords %) {:x x :y y}) (get-in @state/game [:units :balls])))

(defn pickup-ball [x y]
  (let [selected-unit (selected-unit)
        ball          (unit? x y :balls)
        updated-unit  (conj selected-unit {:ball true})
        team          (filter #(not= selected-unit %) (active-team))]
    (if (ball-in-front-of-unit? ball selected-unit)
      (do
        (swap! state/game assoc-in [:units :balls] (balls-without x y))
        (swap! state/game assoc-in [:units (:turn @state/game)] (conj team updated-unit))))))

(defn selected? [x y]
  (= {:x x :y y} (:coords (selected-unit))))


(defn unit-class [x y]
  (let [blue-unit (unit? x y :blue-team)
        red-unit  (unit? x y :red-team)
        ball-unit (unit? x y :balls)]
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

     (unit? x y :balls)
     "ball")

     (cond
      (selected? x y)
      (str " " "selected")))))

(defn in-range? [x y id]
  (and
   (or
     (and
      (= :blue-team (:turn @state/game))
      (<= y 7))
     (and
      (= :red-team (:turn @state/game))
      (>= y 4)))
   (<=
    (reduce
     +
     (map distance [x y] (vals (:coords (active-team-unit-by-id id)))))
    move-range)))
