(ns dodgeball.core-test
  (:require [cemerick.cljs.test :refer-macros [is are deftest testing use-fixtures done]]
            [reagent.core :as reagent :refer [atom]]
            [dodgeball.unit :as unit]))


(def attack-range 4)


(deftest attack-range
         (is (= 4 unit/attack-range)))

