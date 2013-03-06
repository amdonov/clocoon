(ns clocoon.cache-test
  (:use clojure.test)
  (:require [clocoon.sax :as sax]
            [clocoon.filter.xsl :as xsl]
            [clocoon.cache.resource]
            [clocoon.cache.core :as cache]
            [clocoon.cache.pipeline :as pipeline]
            [clocoon.resource :as resource]
            [clocoon.source :as source]
            [clocoon.serializer :as serializer])
  (:import (clocoon.cache.resource CachedResource)))

(def html-transform (xsl/filter "test/resources/test.xsl"))

(pipeline/use-file-cache)

(deftest xml-test
         (testing "reading and serializing out XML with cache"
                  (let [res (sax/pipeline 
                              "test/resources/test.xml" serializer/xml)]
                    (is (= "text/xml" (get (:headers res) "Content-Type"))))))

(deftest html-test
         (testing "reading XML, one XSL filter, and serializing out HTML with cache"
                  (let [res (sax/pipeline 
                              "test/resources/test.xml" 
                              serializer/html (html-transform))]
                    (is (= "text/html" (get (:headers res) "Content-Type"))))))

(deftest resource-cache-test 
         (testing "reading caching a file"
                  (let [res (CachedResource. "test/resources/test.xml")
                        src (resource/fetch res)
                        mtime (source/last-modified src)]
                    (is (= true (cache/cache-valid? 
                                  res mtime)))
                    (is (nil? (resource/fetch res mtime)))
                    (is (not (nil? (resource/fetch res)))))))
