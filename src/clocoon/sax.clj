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

(defn do-pipeline
  "Chain together an XMLReader, ContentHandler, and zero or more XMLFilters and parse and InputSource. Don't call this directly. It's exposed for cache implementations."
  [resource handler filters]
  (let [s (resource/fetch resource)
        inputSource (source/input-source s)
        reader (reduce wrap-reader (source/reader s) filters)]
    (.setContentHandler reader handler)
    (if (instance? LexicalHandler handler)
      (.setProperty reader "http://xml.org/sax/properties/lexical-handler"
                    handler))
    (.parse reader inputSource)))

(defn pipeline
  "Run an XML pipeline and return the result in a map that acceptable for ring/compojure"
  [resource serializer & filters]
  (let [os (ByteArrayOutputStream.)]
    (do-pipeline resource (serializer/handler serializer os) filters)
    {:body (ByteArrayInputStream. (.toByteArray os))
     :headers {"Content-Type" (serializer/content-type serializer)}}))
