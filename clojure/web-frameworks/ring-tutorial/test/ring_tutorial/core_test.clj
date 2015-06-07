(ns ring-tutorial.core-test
  (:require [clojure.test :refer :all]
            [ring-tutorial.core :refer :all]))

(deftest clojure-test-fw-test
  (testing "Is clojure-test framework is working."
    (is (= 1 1))))
