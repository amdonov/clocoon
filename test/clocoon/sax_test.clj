(ns clocoon.sax-test
  (:use clojure.test)
  (:require [clocoon.sax :as sax]
            [clocoon.filter.xsl :as xsl]
            [clocoon.serializer :as serializer]))

(def html-transform (xsl/filter "test/resources/test.xsl"))

(deftest xml-test
         (testing "reading and serializing out XML"
                  (let [res (sax/pipeline 
                              "test/resources/test.xml" serializer/xml)]
                    (is (= "text/xml" (get (:headers res) "Content-Type"))))))

(deftest html-test
         (testing "reading XML, one XSL filter, and serializing out HTML"
                  (let [res (sax/pipeline 
                              "test/resources/test.xml" 
                              serializer/html (html-transform))]
                    (is (= "text/html" (get (:headers res) "Content-Type"))))))

