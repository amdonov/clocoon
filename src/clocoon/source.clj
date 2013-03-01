(ns clocoon.source
  (:refer-clojure :exclude [resolve])
  (:use [clojure.tools.logging :only (info)]
        [clocoon.core])
  (:require [clocoon.reader :as reader]
            [clocoon.serialize :as serialize])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream File)
           (java.net URL)
           (java.nio.file Files)
           (org.xml.sax InputSource XMLReader)
           (org.xml.sax.ext LexicalHandler)))

(defrecord Source
  [reader inputSource mtime])

(defrecord CachedSource
  [data ctime])

(defprotocol PResource
  (systemId [this])
  (fetch [this] [this mtime]))

(defn- with-infoset-cache 
  "Cache source in memory as byte arrays of fastinfoset content"
  [cache resource]
  (let [resource (:resource resource)
        cId (cache-id resource)
        r (cache cId)
        ctime (if (nil? r) nil (:ctime r))
        source (if (nil? ctime)
                 (fetch resource)
                 (fetch resource ctime))]
    (if (nil? source)
      cache
      (let [{:keys [reader inputSource mtime]} source
            bos (ByteArrayOutputStream.)
            handler (serialize/create serialize/infoset bos)]
        (.setContentHandler reader handler)
        (if (instance? LexicalHandler handler)
            (.setProperty reader 
                          "http://xml.org/sax/properties/lexical-handler"
                          handler))
        (.parse reader inputSource)
        (info "Caching infoset copy of" (systemId resource))
        (assoc cache cId (CachedSource. (.toByteArray bos) mtime))))))

(def ^{:private true} source-cache (atom {}))

(defrecord CachedResource
  [resource]
  PCacheable
  (cache-id [this] (cache-id (:resource this)))
  (cache-valid? 
    [this ctime]
    ; Check the underlining resource to see if
    ; it's modified. If it is then drop it from the cache
    (let [res (cache-valid? (:resource this) ctime)]
      (if-not res
        (swap! source-cache (fn
                              [cache cache-id]
                              (dissoc cache cache-id)) (cache-id this)))
      res))
  PResource
  (systemId [this] (systemId (:resource this)))
  (fetch [this]
    (fetch this nil))
  (fetch [this mtime]
    (swap! source-cache with-infoset-cache this)
    (let [cache-id (cache-id this)
          r (@source-cache cache-id)
          ctime (:ctime r)]
      (if (and (not (nil? mtime)) (<= ctime mtime))
        nil
        (let [is (InputSource. (ByteArrayInputStream. (:data r)))]
          (.setSystemId is (systemId this))
          (Source. (reader/get "application/fastinfoset") is ctime))))))

(extend-type File
  PCacheable
  (cache-id [this] (str this))
  (cache-valid? [this ctime]
    {:pre [(.isFile this)]}
    (<= (.lastModified this) ctime))
  PResource
  (systemId [this] (str this))
  (fetch
    ([this]
     {:pre [(.isFile this)]}
     (let [reader (reader/get (Files/probeContentType (.toPath this)))
           mtime (.lastModified this)]
       (Source. reader (InputSource. (systemId this)) mtime)))
    ([this mtime]
     (if-not (cache-valid? this mtime)
       (fetch this)
       nil))))

(extend-type URL
  PCacheable
  (cache-id [this] (str this))
  (cache-valid? [this ctime]
    (let [conn (.openConnection this)]
      (.setRequestMethod conn "HEAD")
      (.setIfModifiedSince conn ctime)
      (case (.getResponseCode conn)
        304 true
        false)))
  PResource
  (cacheId [this] (str this))
  (systemId [this] (str this))
  (fetch
    ([this]
     (fetch this nil))
    ([this mtime]
     (let [conn (.openConnection this)]
       (if (not (nil? mtime))
         (.setIfModifiedSince conn mtime))
       (case (.getResponseCode conn)
         304 nil
         ;; TODO handle additional response codes
         (let [ctype (.replaceFirst (.getContentType conn) ";.*" "")
               reader (reader/get ctype)
               ;; It's OK to do this. The stream will be closed by the
               ;; reader as part of end-of-parse cleanup.
               is (InputSource. (.getInputStream conn))
               mtime (.getLastModified conn)]
           (.setSystemId is (systemId this))
           (Source. reader is mtime)))))))

(defn- resolve
  [systemId]
  (let [url (.toURL (.resolve (.toURI (File. "")) systemId))]
    (case (.getProtocol url)
      "file" (File. (.getPath url))
      url)))

(defn- cache-resolve
  [systemId]
  (CachedResource. (resolve systemId)))

(extend-type String
  PCacheable
  (cache-id [this] this)
  (cache-valid? [this ctime]
    (cache-valid? (resolve this) ctime))
  PResource
  (systemId [this] this)
  (fetch
    ([this]
     (fetch (cache-resolve this)))
    ([this mtime]
     (fetch (cache-resolve this) mtime))))
