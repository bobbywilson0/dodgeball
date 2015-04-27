(ns dodgeball.combat-test
  (:require [cemerick.cljs.test :refer-macros [is are deftest testing use-fixtures done]]
            [reagent.core :as reagent :refer [atom]]
            [dodgeball.combat :as combat]))

(deftest test-trajectory
         (is (= {:x 2 :y 2} (combat/trajectory {:x 0 :y 0} {:x 5 :y 5} 3))))

(deftest test-horizontal-trajectory
         (is (= {:x 4 :y 0} (combat/trajectory {:x 0 :y 0} {:x 4 :y 0} 4))))