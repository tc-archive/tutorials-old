(ns myapp.core-test
  (:require [clojure.test :refer :all]
            [myapp.core :refer :all]))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(deftest a-test
  (testing "Simple test."
    (is (= 1 1))))
