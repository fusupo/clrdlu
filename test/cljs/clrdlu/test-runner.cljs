(ns clrdlu.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [clrdlu.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'clrdlu.core-test))
    0
    1))
