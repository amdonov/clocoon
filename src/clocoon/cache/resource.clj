(ns clocoon.cache.resource
  (:use [clojure.tools.logging :only (info)]
        [clocoon.cache.core]
        [clocoon.resource])
  (:require [clocoon.reader :as reader]
            [clocoon.serializer :as serializer]
            [clocoon.resolver :as resolver]
            [clocoon.source :as source])
  (:import (clocoon.source Source)
           (java.io ByteArrayInputStream ByteArrayOutputStream File)
           (java.net URL)
           (java.nio.file Files)
           (org.xml.sax InputSource XMLReader)
           (org.xml.sax.ext LexicalHandler)))

(extend-protocol PCacheable
  File
  (cache-id [this] (str this))
  (cache-valid? [this ctime]
    {:pre [(.isFile this)]}
    (<= (.lastModified this) ctime))
  URL
  (cache-id [this] (str this))
  (cache-valid? [this ctime]
    (let [conn (.openConnection this)]
      (.setRequestMethod conn "HEAD")
      (.setIfModifiedSince conn ctime)
      (case (.getResponseCode conn)
        304 true
        false)))
  String
  (cache-id [this] this)
  (cache-valid? [this ctime]
    (cache-valid? (resolver/resolve this) ctime)))

(defrecord CachedSource
  [data systemId ctime])

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
      (let [reader (source/reader source)
            inputSource (source/input-source source)
            mtime (source/last-modified source)
            bos (ByteArrayOutputStream.)
            handler (serializer/handler serializer/infoset bos)
            systemId (.getSystemId inputSource)]
        (.setContentHandler reader handler)
        (if (instance? LexicalHandler handler)
            (.setProperty reader 
                          "http://xml.org/sax/properties/lexical-handler"
                          handler))
        (.parse reader inputSource)
        (info "Caching infoset copy of" systemId)
        (assoc cache cId (CachedSource. (.toByteArray bos) systemId mtime))))))

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
          (.setSystemId is (:systemId r))
          (Source. (reader/get "application/fastinfoset") is ctime))))))

