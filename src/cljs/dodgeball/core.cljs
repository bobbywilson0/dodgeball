(ns dodgeball.core
  (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [cljs.core.async :refer [put! chan <!]]
              [cljsjs.react :as react])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History))

(def game
  (atom
   {:units
    {:blue-team
     [{:id 1 :coords {:x 1 :y 0}}
      {:id 2 :coords {:x 3 :y 0}}
      {:id 3 :coords {:x 5 :y 0}}
      {:id 4 :coords {:x 7 :y 0}}
      {:id 5 :coords {:x 9 :y 0}}]
     :red-team
     [{:id 1 :coords {:x 1, :y 10}}
      {:id 2 :coords {:x 3, :y 10}}
      {:id 3 :coords {:x 5, :y 10}}
      {:id 4 :coords {:x 7, :y 10}}
      {:id 5 :coords {:x 9, :y 10}}]
     :balls
     [{:id 1 :coords {:x 1  :y 5}}
      {:id 2 :coords {:x 3 :y 5}}
      {:id 3 :coords {:x 5 :y 5}}
      {:id 4 :coords {:x 7 :y 5}}
      {:id 5 :coords {:x 9 :y 5}}]}
     :turn :blue-team
     :actions 0}))

;; -------------------------
;; Views

(def board-length (range 0 11))

(defn border-top [y]
  (cond
    (or (= y 4) (= y 7)) "middle-top"))

(defn all-unit-coords-for [unit-type]
  (get-in @game [:units unit-type]))

(defn unit? [x y unit-type]
  (first
   (filter #(= {:x x :y y} (:coords %)) (all-unit-coords-for unit-type))))

(defn active-team []
  (let [game @game]
  ((:turn game) (:units game))))

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
    (swap! game assoc-in [:units (:turn @game)] (conj team updated-unit))))

(defn deselect-unit []
  (let [selected-unit (selected-unit)
        updated-unit
        (conj
         selected-unit
         {:selected false})
        team
        (filter #(not= selected-unit %) (active-team))]
    (swap! game assoc-in [:units (:turn @game)] (conj team updated-unit))))

(defn move-unit [x y]
    (let [selected-unit (selected-unit)
        updated-unit
        (conj
         selected-unit
         {:coords {:x x :y y}})
        team
        (filter #(not= selected-unit %) (active-team))]
      (swap! game assoc-in [:units (:turn @game)] (conj team updated-unit))))

(defn ball-in-front-of-unit? [ball unit]
  (let [ball-x (:x (:coords ball))
        ball-y (:y (:coords ball))
        unit-y (:y (:coords unit))
        unit-x (:x (:coords unit))]
    (and
     (= ball-x unit-x)
     (or
      (and
       (= (:turn @game) :blue-team)
       (= unit-y (dec ball-y)))
      (and
       (= (:turn @game) :red-team)
       (= unit-y (inc ball-y)))))))

(defn balls-without [x y]
  (filter #(not= (:coords %) {:x x :y y}) (get-in @game [:units :balls])))

(defn pickup-ball [x y]
  (let [selected-unit (selected-unit)
        ball          (unit? x y :balls)
        updated-unit  (conj selected-unit {:ball true})
        team          (filter #(not= selected-unit %) (active-team))]
    (if (ball-in-front-of-unit? ball selected-unit)
      (do
        (swap! game assoc-in [:units :balls] (balls-without x y))
        (swap! game assoc-in [:units (:turn @game)] (conj team updated-unit))))))

(defn selected? [x y]
  (= {:x x :y y} (:coords (selected-unit))))


(defn unit-class [x y]
  (str
    (cond
     (unit? x y :blue-team)
     "blue-team"

     (unit? x y :red-team)
     "red-team"

     (unit? x y :balls)
     "ball")

     (cond
      (selected? x y)
      (str " " "selected"))))

(defn in-range? [x y id]
  (and
   (or
     (and
      (= :blue-team (:turn @game))
      (<= (:y y) 7))
     (and
      (= :red-team (:turn @game))
      (>= (:y y) 4)))
   (<=
    (reduce
     +
     (map distance [x y] (vals (:coords (active-team-unit-by-id id))))
    move-range))))

(defn determine-action [x y]
    (cond
     (and
      (= nil (selected-unit))
      (unit? x y (:turn @game)))
     {:type :select-unit
      :id (:id (unit? x y (:turn @game)))
      :coords {:x x :y y}}

     (or
       (unit? x y (:turn @game))
       (not (in-range? x y (:id (selected-unit)))))
     {:type :deselect-unit
      :id (:id (unit? x y (:turn @game)))
      :coords {:x x :y y}}

     (boolean (unit? x y :balls))
     {:type :pickup-ball
      :id (:id (unit? x y :balls))
      :coords {:x x :y y}}


     (in-range? x y (:id (unit? x y (:turn @game))))
     {:type :move-unit
      :id (:id (unit? x y (:turn @game)))
      :coords {:x x :y y}}

     :else
       (println x y (:turn @game))))

(defn game-board [{:keys [:actions]}]
  [:div
    [:h1 (str (:turn @game))]
    [:h1 (:actions @game)]
    [:table
     (doall (for [y board-length]
       [:tr {:key y :class (border-top y)}
        (doall (for [x board-length]
           [:td {:key (str x y)
                 :class (unit-class x y)
                 :on-click #(put! actions (determine-action x y))}]))]))]])

;; -------------------------
;; Initialize app
(defn mount-root []
  (let [ch (chan)]
  (reagent/render
   [game-board {:actions ch}]
   (.getElementById js/document "app"))
  (go
   (loop []
     (let [event (<! ch)]
       (println event)
        (case (:type event)
          :select-unit   (select-unit (:id event))
          :deselect-unit (deselect-unit)
          :move-unit     (do
                           (move-unit   (:x (:coords event)) (:y (:coords event)))
                           (swap! game update-in [:actions] inc)
                           (deselect-unit))
          :pickup-ball   (do
                           (pickup-ball (:x (:coords event)) (:y (:coords event)))
                           (swap! game update-in [:actions] inc)
                           (deselect-unit)))
       (recur))))))


(defn init! []
  (mount-root))
