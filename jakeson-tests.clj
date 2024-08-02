(load-file "jakeson-worker.clj")
(ns jakeson-tests
  (:require [clojure.test :as t]
            [jakeson-worker :as jakeson]))

(t/deftest not-blank?-test
  (t/testing "blank string"
    (t/is (false? (jakeson/not-blank? ""))))
  (t/testing "white space string is blank")
    (t/is (false? (jakeson/not-blank? " ")))
  (t/testing "legit not-blank"
    (t/is (true? (jakeson/not-blank? "not blank")))))

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

(t/deftest read-w-prompt-test
  (t/testing "with default, empty response"
    (t/is (= "world"
             (jakeson/read-w-prompt "[TEST] hello" "world" #(str "")))))
  (t/testing "without default, empty response"
    (t/is (= ""
             (jakeson/read-w-prompt "[TEST] hello" "" #(str ""))))))


(t/run-tests)
