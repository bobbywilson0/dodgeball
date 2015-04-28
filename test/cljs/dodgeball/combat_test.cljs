(ns dodgeball.combat-test
  (:require [cemerick.cljs.test :refer-macros [is are deftest testing use-fixtures done]]
            [reagent.core :as reagent :refer [atom]]
            [dodgeball.combat :as combat]))

(deftest test-trajectory-angle
         (is (= {:x 3 :y 3} (combat/trajectory {:x 0 :y 0 :type :blue} {:x 5 :y 5} 3))))

(deftest test-trajectory-reverse-angle
         (is (= {:x 2 :y 2} (combat/trajectory {:x 5 :y 5 :type :red} {:x 0 :y 0} 3))))


(deftest test-horizontal-trajectory
         (is (= {:x 4 :y 0} (combat/trajectory {:x 0 :y 0 :type :blue} {:x 4 :y 0} 4))))