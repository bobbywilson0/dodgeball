(ns dodgeball.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [dodgeball.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'dodgeball.core-test))
    0
    1))
