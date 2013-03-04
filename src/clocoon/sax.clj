(ns clocoon.sax
  (:use [clojure.tools.logging :only (info error)]
        [clocoon.core]
        [clocoon.filter.core])
  (:require [clocoon.source :as source]
            [clocoon.serialize :as serialize])
  (:import (org.xml.sax.ext LexicalHandler)
           (java.io ByteArrayInputStream ByteArrayOutputStream)))

(defn- wrap-reader 
  "Wrap an XMLReader with an XMLFilter"
  [reader xmlfilter]
  (let [f (get-filter xmlfilter)]
    (.setParent f reader)
    f))

(defn get-parser
  [resource]
  (let [res (source/fetch resource)
        reader (source/reader res)
        inputSource (source/input-source res)]
    (fn [handler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader handler)
        (if (instance? LexicalHandler handler)
          (.setProperty reader "http://xml.org/sax/properties/lexical-handler"
                        handler))
        (.parse reader inputSource)))))

(defn do-pipeline [resource serializer filters]
  (let [parser (get-parser resource)]
    (apply parser serializer filters)))

(defn pipeline [resource serializer & filters]
  (let [os (ByteArrayOutputStream.)]
    (do-pipeline resource (serialize/create serializer os) filters)
    {:body (ByteArrayInputStream. (.toByteArray os))
     :headers {"Content-Type" (serialize/content-type serializer)}}))
