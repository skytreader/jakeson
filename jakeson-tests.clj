(load-file "jakeson.clj")
(ns jakeson-tests
  (:require [clojure.test :as t]
            [jakeson :as jakeson]))

(t/deftest bool?-test
  (t/testing "truthy values are bool"
    (for [truthy-val jakeson/TRUTHY]
      (t/is (jakeson/truthy? truthy-val)))))

(t/run-tests)
