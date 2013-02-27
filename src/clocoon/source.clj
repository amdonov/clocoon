(ns clocoon.source
  (:refer-clojure :exclude [resolve])
  (:use [clojure.tools.logging :only (info)])
  (:require [clocoon.reader :as reader]
            [clocoon.serialize :as serialize])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream File)
           (java.net URL)
           (java.nio.file Files)
           (org.xml.sax InputSource)))

(defrecord Source
  [reader inputSource mtime])

(defrecord CachedSource
  [data ctime])

(defprotocol Resource
  (modified? [this mtime])
  (cacheId [this])
  (systemId [this])
  (fetch [this] [this mtime]))

(defn- with-infoset-cache 
  "Cache source in memory as byte arrays of fastinfoset content"
  [cache resource]
  (let [resource (:resource resource)
        cId (cacheId resource)
        r (cache cId)
        ctime (if (nil? r) nil (:ctime r))
        source (if (nil? ctime)
                 (fetch resource)
                 (fetch resource ctime))]
    (if (nil? source)
      cache
      (let [{:keys [reader inputSource mtime]} source
            bos (ByteArrayOutputStream.)]
        (.setContentHandler reader ((:constructor serialize/infoset) bos))
        (.parse reader inputSource)
        (info "Caching infoset copy of" (systemId resource))
        (assoc cache cId (CachedSource. (.toByteArray bos) mtime))))))

(def ^{:private true} source-cache (atom {}))

(defrecord CachedResource
  [resource]
  Resource
  (cacheId [this] (cacheId (:resource this)))
  (systemId [this] (systemId (:resource this)))
  (modified? 
    [this mtime]
    ; Check the underlining resource to see if
    ; it's modified. If it is then drop it from the cache
    (let [res (modified? (:resource this) mtime)]
      (if res
          (swap! source-cache (fn
                                [cache systemId]
                                (dissoc cache systemId)) (cacheId this)))
      res))
  (fetch [this]
    (fetch this nil))
  (fetch [this mtime]
    (swap! source-cache with-infoset-cache this)
    (let [cacheId (cacheId this)
          r (@source-cache cacheId)
          ctime (:ctime r)]
      (if (and (not (nil? mtime)) (<= ctime mtime))
        nil
        (let [is (InputSource. (ByteArrayInputStream. (:data r)))]
          (.setSystemId is (systemId this))
          (Source. (reader/get "application/fastinfoset") is ctime))))))

(extend-type File
  Resource
  (cacheId [this] (str this))
  (systemId [this] (str this))
  (modified?
    [this mtime]
    {:pre [(.isFile this)]}
    (> (.lastModified this) mtime))
  (fetch
    ([this]
     {:pre [(.isFile this)]}
     (let [reader (reader/get (Files/probeContentType (.toPath this)))
           mtime (.lastModified this)]
       (Source. reader (InputSource. (systemId this)) mtime)))
    ([this mtime]
     (if (modified? this mtime)
       (fetch this)
       nil))))

(extend-type URL
  Resource
  (cacheId [this] (str this))
  (systemId [this] (str this))
  (modified?
    [this mtime]
    (let [conn (.openConnection this)]
      (.setRequestMethod conn "HEAD")
      (.setIfModifiedSince conn mtime)
      (case (.getResponseCode conn)
        304 false
        true)))
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
  Resource
  (cacheId [this] this)
  (systemId [this] this)
  (modified? [this mtime]
    (modified? (cache-resolve this) mtime))
  (fetch
    ([this]
     (fetch (cache-resolve this)))
    ([this mtime]
     (fetch (cache-resolve this) mtime))))
