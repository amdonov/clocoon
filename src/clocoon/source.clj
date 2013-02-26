(ns clocoon.source
  (:refer-clojure :exclude [resolve])
  (:use [clojure.tools.logging :only (info)])
  (:require [clocoon.reader :as reader])
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
  (fetch [this] [this mtime]))

(defrecord CachedResource
  [resource]
  Resource
  (modified? [this mtime]
    (println "Using Cache")
    (modified? (:resource this) mtime))
  (fetch [this]
    (println "Using Cache")
    (fetch (:resource this)))
  (fetch [this mtime]
    (println "Using Cache")
    (fetch (:resource this) mtime)))

(extend-type File
  Resource
  (modified?
    [this mtime]
    {:pre [(.isFile this)]}
    (> (.lastModified this) mtime))
  (fetch
    ([this]
     {:pre [(.isFile this)]}
     (let [reader (reader/get (Files/probeContentType (.toPath this)))
           mtime (.lastModified this)]
       (Source. reader (InputSource. (str this)) mtime)))
    ([this mtime]
     (if (modified? this mtime)
       (fetch this)
       nil))))

(extend-type URL
  Resource
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
           (.setSystemId is (str this))
           (Source. reader is mtime)))))))

(defn- resolve
  [systemId]
  (let [url (.toURL (.resolve (.toURI (File. "")) systemId))]
    (case (.getProtocol url)
      "file" (File. (.getPath url))
      url)))

(defn- cache-resolve
  [systemId]
  (CachedResource.(resolve systemId)))

(extend-type String
  Resource
  (modified? [this mtime]
    (modified? (cache-resolve this) mtime))
  (fetch
    ([this]
     (fetch (cache-resolve this)))
    ([this mtime]
     (fetch (cache-resolve this) mtime))))




