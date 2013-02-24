(ns clocoon.sax
  (:use [clojure.tools.logging :only (info error)])
  (:require [clocoon.io :as io]
            [clocoon.serialize :as ser]
            [clocoon.source :as source])
  (:import (clocoon.source Source)
           (javax.xml.transform TransformerFactory URIResolver)
           (javax.xml.transform.sax SAXSource SAXTransformerFactory)
           (org.xml.sax InputSource)
           (java.io File ByteArrayInputStream BufferedOutputStream
                    FileOutputStream ByteArrayOutputStream)
           (java.net URI)))

(defprotocol CachedFilter
  (cache-valid? [this ctime])
  (cache-id [this]))

(defrecord XSLFilter [xmlfilter mtime cacheId]
  CachedFilter
  (cache-id [this]
    (:cacheId this))
  (cache-valid? [this ctime]
    (> ctime mtime)))

(defn- wrap-reader 
  "Wrap an XMLReader with an XMLFilter"
  [reader xmlfilter]
  (let [f (if (instance? XSLFilter xmlfilter)
            (:xmlfilter xmlfilter)
            xmlfilter)]
    (.setParent f reader)
    f))

(defrecord CachedResource [data mtime])

(def ^{:private true} resource-cache (atom {}))

(defn- with-infoset-cache [cache systemId]
  (let [r (cache systemId)]
    (let [ltime (if (nil? r) nil (:mtime r))]
      (let [resource (source/get-source systemId ltime)]
        (if (nil? resource) 
          cache
          (let [{:keys [reader inputSource mtime]} resource 
                bos (ByteArrayOutputStream.)]
            (.setContentHandler reader (ser/create-infoset-serializer bos))
            (.parse reader inputSource)
            (info "Caching infoset copy of" systemId)
            (assoc cache systemId (CachedResource. 
                                    (.toByteArray bos) mtime))))))))

(defn- get-cached-resource [systemId]
  (swap! resource-cache with-infoset-cache systemId)
  (let [r (@resource-cache systemId)]
    (let [is (InputSource. (ByteArrayInputStream. (:data r)))]
      (.setSystemId is systemId)
      (Source. 
        (source/get-reader "application/fastinfoset") is (:mtime r)))))

(defn get-parser
  [systemId]
  (let [{:keys [reader inputSource]} (get-cached-resource systemId)]
    (fn [contentHandler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader contentHandler)
        (.parse reader inputSource)))))

(defn get-source [systemId]
  (let [{:keys [reader inputSource mtime]} (get-cached-resource systemId)]
    {:source (SAXSource. reader inputSource)
     :mtime mtime}))

(defrecord Template [obj deps ctime])

(def ^{:private true} templates-cache (atom {}))

(defn- get-templates [systemId]
  (let [tf (TransformerFactory/newInstance)]
    (with-local-vars [deps #{systemId} ctime 0]
      (.setURIResolver tf (reify 
                            URIResolver
                            (resolve [this href, base]
                              (let [systemId (str (.resolve (URI. base) href))
                                    source (get-source systemId)]
                                (var-set deps (conj (var-get deps) systemId))
                                (var-set ctime (max (:mtime source)
                                                    (var-get ctime)))
                                (:source source)))))
      (let [source (get-source systemId)] 
        (Template. (.newTemplates tf (:source source))
                   (var-get deps)
                   (max (var-get ctime) (:mtime source)))))))

(defn- cached-resource-valid? [ctime systemId]
  (let [mtime (:mtime (get-cached-resource systemId))]
    (<= mtime ctime)))

(defn- cached-template-valid? [template]
  (let [{:keys [ctime deps]} template]
    (every? (partial cached-resource-valid? ctime) deps)))

(defn- with-templates-cache [cache systemId]
  (let [t (cache systemId)]
    (if (or (nil? t) (not (cached-template-valid? t)))
      (do
        (info "Caching templates for" systemId)
        (assoc cache systemId (get-templates systemId)))
      cache)))

(defn- get-cached-templates [systemId]
  (swap! templates-cache with-templates-cache systemId)
  (@templates-cache systemId))

(defn- get-xsl-filter-internal [systemId params]
  (let [templates (get-cached-templates systemId)]
    (let [f (.. (SAXTransformerFactory/newInstance) 
              (newXMLFilter (:obj templates)))
          ctime (:ctime templates)]
      (let [t (.getTransformer f)]
        (if (> 0 (count params))
          (apply (fn [k] (.setParameter t k (params k)))
                 (keys params))))
      (XSLFilter. f ctime (str systemId params)))))

(defn get-xsl-filter [systemId]
  (fn 
    ([]
     (get-xsl-filter-internal systemId {}))
    ([params]
     (get-xsl-filter-internal systemId params))))

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
