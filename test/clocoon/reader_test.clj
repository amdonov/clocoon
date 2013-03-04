(ns clocoon.reader-test
  (:use clojure.test)
  (:require [clocoon.reader :as reader]))

(deftest xml-test
         (testing "reading xml"
                  (let [reader (reader/get "text/xml")]
                    (is (not (nil? reader))))))
