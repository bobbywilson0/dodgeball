(ns dodgeball-r.core
  (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [cljs.core.async :refer [put! chan <!]]
              [cljsjs.react :as react])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   )
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

(defn selected-unit []
  (:coords
   (first
    (filter :selected ((:turn @game) (:units @game))))))

(defn active-team []
  (let [game @game]
  ((:turn game) (:units game))))

(defn select-unit [id]
  (println (first (filter #(= id (:id %)) (active-team)))))

(defn selected? [x y]
  (= {:x x :y y} (selected-unit)))


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

(defn determine-action [x y]

    (cond
     (and
      (= nil (selected-unit))
      (unit? x y (:turn @game)))
       {:type :select-unit :id (:id (unit? x y (:turn @game))) :coords {:x x :y y}}
     :else
       (println x y (:turn @game))))

(defn game-board [{:keys [:actions]}]
  [:div
    [:h1 (str (:turn @game))]
    [:table
     (doall (for [y board-length]
       [:tr {:key y :class (border-top y)}
        (doall (for [x board-length]
           [:td {:key (str x y) :class (unit-class x y) :on-click #(put! actions (determine-action x y))}]))]))]])

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
        (case (:type event)
          :select-unit   (select-unit (:id event)))
;;          :deselect-unit (deselect-unit owner)
;;          :move-unit     (move-unit app owner (:coords event))
;;          :pickup-ball   (pickup-ball app owner (:coords event)))

       (recur))))))



(defn init! []
  (mount-root))
