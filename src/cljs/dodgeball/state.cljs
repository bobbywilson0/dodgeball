(ns dodgeball.state
  (:require [reagent.core :refer [atom]]))

(def game
  (atom
   {:units
    [{:id 1 :type :blue :x 0 :y 0}
     ;{:id 2 :type :blue :x 0 :y 1}
     {:id 3 :type :blue :x 0 :y 2}
     ;{:id 4 :type :blue :x 0 :y 3}
     {:id 5 :type :blue :x 0 :y 4}
     {:id 6 :type :red :x 8, :y 0}
     ;{:id 7 :type :red :x 8, :y 1}
     {:id 8 :type :red :x 8, :y 2}
     ;{:id 9 :type :red :x 8, :y 3}
     {:id 10 :type :red :x 8, :y 4}
     {:id 11 :type :ball :x 4 :y 0}
     ;{:id 12 :type :ball :x 4 :y 1}
     {:id 13 :type :ball :x 4 :y 2}
     ;{:id 14 :type :ball :x 4 :y 3}
     {:id 15 :type :ball :x 4 :y 4}]
    :blue-bench []
    :red-bench []
    :turn :red
    :actions 0}))

(defn defense []
  (if (= (:turn @game) :red)
    :blue
    :red))

(defn selected-unit []
  (first
    (filter :selected (:units @game))))