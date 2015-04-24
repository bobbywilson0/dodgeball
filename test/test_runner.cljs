(ns test.test-runner
  (:require [cljs.test :as tt]
            [test.cljs.dodgeball.unit-test]))

(enable-console-print!)

(defn runner []
  (tt/run-tests
    (tt/empty-env ::tt/default)
    'test.cljs.dodgeball.unit-test))

(defn ^:export set-print-fn! [f]
  (set! cljs.core.*print-fn* f))
