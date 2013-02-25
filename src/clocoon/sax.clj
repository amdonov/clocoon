(ns clocoon.sax
  (:use [clojure.tools.logging :only (info error)]
        [clocoon.filter.core])
  (:require [clocoon.io :as io]
            [clocoon.source :as source])
  (:import (clocoon.source Source)
           (javax.xml.transform TransformerFactory URIResolver)
           (javax.xml.transform.sax SAXSource SAXTransformerFactory)
           (org.xml.sax InputSource)
           (java.io File ByteArrayInputStream BufferedOutputStream
                    FileOutputStream ByteArrayOutputStream)
           (java.net URI)))

(defn- wrap-reader 
  "Wrap an XMLReader with an XMLFilter"
  [reader xmlfilter]
  (let [f (get-filter xmlfilter)]
    (.setParent f reader)
    f))

(defn- cached-resource-valid? [ctime systemId]
  (let [mtime (:mtime (source/get systemId))]
    (<= mtime ctime)))

(defn get-parser
  [systemId]
  (let [{:keys [reader inputSource]} (source/get systemId)]
    (fn [contentHandler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader contentHandler)
        (.parse reader inputSource)))))

(defn- do-pipeline [systemId serializer-factory filters]
  (let [parser (get-parser systemId)
        file (io/make-cache-file)
        serializer (:constructor serializer-factory) ]
    (with-open [os (BufferedOutputStream. (FileOutputStream. file))]
      (apply parser (serializer os) filters))
    file))

(def ^{:private true} pipeline-cache (atom io/cache))

(defn- cached-filter-valid? [ctime f]
  (if (satisfies? CachedFilter f)
    (cache-valid? f ctime)
    true))

(defn- cached-pipeline-valid? [ctime systemId filters]
  (and 
    (cached-resource-valid? ctime systemId)
    (every? (partial cached-filter-valid? ctime) filters)))

(defn- get-pipeline-cache-id [systemId serializer-factory filters]
  (str systemId (:cacheId serializer-factory) (reduce str (map :cacheId (filter (partial satisfies? CachedFilter) filters)))))

(defn- with-pipeline-cache [cache systemId serializer-factory filters]
  (let [cacheId (get-pipeline-cache-id systemId serializer-factory filters)]
    (let [f (cache cacheId)]
      (if (or (nil? f)
              (not (.exists f))
              (not (cached-pipeline-valid? 
                     (.lastModified f) 
                     systemId
                     filters)))
        (do 
          (if (not (nil? f))
            (.delete f))
          (let [file (do-pipeline systemId serializer-factory filters)]
            (.write io/journal (str cacheId "||" file "\n"))
            (.flush io/journal)
            (assoc cache cacheId file)))
        cache))))

(defn pipeline [systemId serializer-factory & filters]
  (swap! pipeline-cache with-pipeline-cache systemId serializer-factory filters)
  (@pipeline-cache (get-pipeline-cache-id systemId serializer-factory filters)))
