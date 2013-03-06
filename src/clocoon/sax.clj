(ns clocoon.sax
  (:require [clocoon.source :as source]
            [clocoon.resource :as resource]
            [clocoon.serializer :as serializer]
            [clocoon.filter.core :as filter])
  (:import (org.xml.sax.ext LexicalHandler)
           (java.io ByteArrayInputStream ByteArrayOutputStream)))

(defn- wrap-reader 
  "Wrap an XMLReader with an XMLFilter"
  [reader xmlfilter]
  (let [f (filter/get-filter xmlfilter)]
    (.setParent f reader)
    f))

(defn- get-parser
  [resource]
  (let [s (resource/fetch resource)
        reader (source/reader s)
        inputSource (source/input-source s)]
    (fn [handler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader handler)
        (if (instance? LexicalHandler handler)
          (.setProperty reader "http://xml.org/sax/properties/lexical-handler"
                        handler))
        (.parse reader inputSource)))))

(defn do-pipeline [resource handler filters]
  (let [parser (get-parser resource)]
    (apply parser handler filters)))

(defn pipeline [resource serializer & filters]
  (let [os (ByteArrayOutputStream.)]
    (do-pipeline resource (serializer/handler serializer os) filters)
    {:body (ByteArrayInputStream. (.toByteArray os))
     :headers {"Content-Type" (serializer/content-type serializer)}}))
