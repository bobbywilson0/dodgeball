(ns dodgeball.state
  (:require [reagent.core :refer [atom]]))

(def game
  (atom
   {:units
    [{:id 1 :type :blue :x 0 :y 0}
     {:id 3 :type :blue :x 0 :y 2}
     {:id 5 :type :blue :x 0 :y 4}
     {:id 6 :type :red :x 8, :y 0}
     {:id 8 :type :red :x 8, :y 2}
     {:id 10 :type :red :x 8, :y 4}
     {:id 11 :type :ball :x 4 :y 0 :dead true}
     {:id 13 :type :ball :x 4 :y 2 :dead true}
     {:id 15 :type :ball :x 4 :y 4 :dead true}]
    :blue-bench []
    :red-bench []
    :turn :red
    :actions 0
    :selected-unit nil
    :highlighted-unit nil
    :movement-range nil
    :images-loaded false}))

(defn defense []
  (if (= (:turn @game) :red)
    :blue
    :red))

(defn selected-unit []
  (first
    (filter :selected (:units @game))))