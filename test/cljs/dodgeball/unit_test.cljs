(ns dodgeball.unit-test
  (:require [cemerick.cljs.test :refer-macros [is are deftest testing use-fixtures done]]
            [reagent.core :as reagent :refer [atom]]
            [dodgeball.unit :as unit]))


(def attack-range 4)


(deftest test-attack-range
         (is (= 4 unit/attack-range)))

(deftest test-find-one-unit-by
         (is (= {:id 1 :type :blue :x 0 :y 0} (unit/find-one-unit-by 0 0 :blue {:units
                                                                                 [{:id 1 :type :blue :x 0 :y 0}
                                                                                  {:id 2 :type :red :x 1 :y 1}]}))))

