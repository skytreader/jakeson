(load-file "jakeson-worker.clj")
(ns jakeson-tests
  (:require [clojure.test :as t]
            [jakeson-worker :as jakeson]))

(t/deftest bool?-test
  (t/testing "truthy values are bool"
    (doseq [truthy-val jakeson/TRUTHY]
      (t/is (jakeson/bool? truthy-val))))
  (t/testing "falsey values are bool"
    (doseq [falsey-val jakeson/FALSEY]
      (t/is (jakeson/bool? falsey-val)))))

(t/deftest generate-choices-prompt-test
  (t/testing "without skip option"
    (t/is (= "1:Clojure 2:Java 3:Python"
             (jakeson/generate-choices-prompt ["Clojure", "Java", "Python"] true))))
  (t/testing "with skip option"
    (t/is (= "0:SKIP 1:Clojure 2:Java 3:Python"
             (jakeson/generate-choices-prompt ["Clojure", "Java", "Python"] false)))))


(t/run-tests)
