(ns test.cljs.dodgeball.unit-test
  (:require [cljs.test :refer-macros [deftest async testing is use-fixtures]]
            [dodgeball.unit :as unit]))

(enable-console-print!)

(deftest adder
         (testing "Does the adder add?"
                  (is (= 4 unit/attack-range))))
