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

(defn get-parser
  [resource]
  (let [{:keys [reader inputSource]} (source/fetch resource)]
    (fn [contentHandler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader contentHandler)
        (.parse reader inputSource)))))

(defn- do-pipeline [resource serializer filters]
  (let [parser (get-parser resource)
        file (io/make-cache-file)
        serializer (:constructor serializer)]
    (with-open [os (BufferedOutputStream. (FileOutputStream. file))]
      (apply parser (serializer os) filters))
    file))

(def ^{:private true} pipeline-cache (atom io/cache))

(defn- cached-filter-valid? [ctime f]
  (if (satisfies? CachedFilter f)
    (cache-valid? f ctime)
    true))

(defn- cached-pipeline-valid? [ctime resource filters]
  (and 
    (not (source/modified? resource ctime))
    (every? (partial cached-filter-valid? ctime) filters)))

(defn- get-pipeline-cache-id [resource serializer filters]
  (str (source/cacheId resource) (:cacheId serializer) (reduce str (map :cacheId (filter (partial satisfies? CachedFilter) filters)))))

(defn- with-pipeline-cache [cache resource serializer filters]
  (let [cacheId (get-pipeline-cache-id resource serializer filters)]
    (let [f (cache cacheId)]
      (if (or (nil? f)
              (not (.exists f))
              (not (cached-pipeline-valid? 
                     (.lastModified f) 
                     resource
                     filters)))
        (do 
          (if (not (nil? f))
            (.delete f))
          (let [file (do-pipeline resource serializer filters)]
            (.write io/journal (str cacheId "||" file "\n"))
            (.flush io/journal)
            (assoc cache cacheId file)))
        cache))))

(defn pipeline [resource serializer & filters]
  (swap! pipeline-cache with-pipeline-cache resource serializer filters)
  (@pipeline-cache (get-pipeline-cache-id resource serializer filters)))
