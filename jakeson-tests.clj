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

(t/deftest read-validated-test
  (t/testing "valid input returns itself"
    (t/is (= "ok"
             (jakeson/read-validated "enter 'ok'" (fn [_] true) #(str "ok")))))
  (t/testing "invalid inputs are repeatedly prompted until a valid input is given"
    (t/is (= "okay"
             (let [retvals ["ok" "0|<" "okay"]
                   index (atom -1)]
               (jakeson/read-validated "enter 'okay'"
                                       #(= % "okay")
                                       (fn [] (do (swap! index inc)
                                                  (get retvals @index))))))))
  (t/testing "exceptions from the validator are considered invalid inputs"
    (t/is (= "okay"
             (let [should-succeed? [false false true]
                   index (atom -1)]
               (jakeson/read-validated "enter 'okay'"
                                       (fn [foo] (if (= foo "okay")
                                                   true
                                                   (throw (RuntimeException. "failed with exception"))))
                                       (fn [] (do (swap! index inc)
                                                  (if (get should-succeed? @index) "okay" "ok")))))))))

(t/deftest read-choices-index
  (t/testing "1-indexed choice is returned"
    (t/is (= 1
             (jakeson/read-choices-index "test"
                                         ["choose me" "not me" "nor me"]
                                         true
                                         #(str "1")))))
  (t/testing "0 is returned for non-required prompts"
    (t/is (= 0
             (jakeson/read-choices-index "test"
                                         ["cats" "seafood" "electric boxing"]
                                         false
                                         #(str "0")))))
  (t/testing "0 can't be chosen for required prompts"
    (t/is (= 2
             (let [user-input ["0" "2"]
                   index (atom -1)]
               (jakeson/read-choices-index "test"
                                           ["dogs" "noodles" "boxing"]
                                           true
                                           (fn [] (do (swap! index inc)
                                                      (get user-input @index))))))))
  (t/testing "out-of-bounds choices are caught"
    (t/is (= 1
             (let [user-input ["3" "1"]
                   index (atom -1)]
               (jakeson/read-choices-index "test"
                                           ["spam" "eggs"]
                                           true
                                           (fn [] (do (swap! index inc)
                                                      (get user-input @index)))))))))

(t/deftest read-choices
  (t/testing "1-indexed choice is returned"
    (t/is (= "choose me"
             (jakeson/read-choices "test"
                                   ["choose me" "not me" "nor me"]
                                   true
                                   #(str "1")))))
  (t/testing "nil is returned if prompt is not required and user skips it"
    (t/is (nil? (jakeson/read-choices "test"
                                      ["cats" "seafood" "electric boxing"]
                                      false
                                      #(str "0")))))
  (t/testing "0 can't be chosen for required prompts"
    (t/is (= "noodles"
             (let [user-input ["0" "2"]
                   index (atom -1)]
               (jakeson/read-choices "test"
                                     ["dogs" "noodles" "boxing"]
                                     true
                                     (fn [] (do (swap! index inc)
                                                (get user-input @index))))))))
  (t/testing "out-of-bounds choices are caught"
    (t/is (= "spam"
             (let [user-input ["3" "1"]
                   index (atom -1)]
               (jakeson/read-choices "test"
                                     ["spam" "eggs"]
                                     true
                                     (fn [] (do (swap! index inc)
                                                (get user-input @index)))))))))

(t/deftest read-multitype
  (t/is (= {"oneOf" (list "string" "integer")}
           (let [VALID-TYPES (filter #(not (= % "multi")) jakeson/TYPES)
                 user-input [(str (inc (.indexOf VALID-TYPES "integer")))
                             (str (inc (.indexOf VALID-TYPES "string")))
                             "0"]
                 index (atom -1)]
             (jakeson/read-multitype "test"
                                     "oneOf"
                                     []
                                     {}
                                     (fn [] (do (swap! index inc)
                                                (get user-input @index))))))))

(t/run-tests)
