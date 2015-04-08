(ns dodgeball.state
  (:require [reagent.core :as reagent :refer [atom]]))

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
     :benches
     {:blue-team []
     :red-team []}
     :turn :red-team
     :actions 0}))
