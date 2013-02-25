(ns clocoon.source
  (:refer-clojure :exclude [get])
  (:use [clojure.tools.logging :only (info)])
  (:require [clocoon.io :as io]
            [clocoon.reader :as reader]
            [clocoon.serialize :as serialize])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream File)
           (org.xml.sax InputSource)))

(defrecord Source 
  [reader inputSource mtime])

(defrecord CachedSource
  [data ctime])

(defn- get-file-source
  "Get a file source located at path or nil if it has not been updated
  since ltime. Throws exceptions if the file does not exist or is in a
  format that cannot be parsed."
  [path ltime]
  (let [f (File. path)]
    (if (and (.exists f) (.isFile f))
      (let [mtime (.lastModified f)]
        (if (or (nil? ltime) (> mtime ltime)) 
          ; Either ltime is nil or the file has changed
          (let [reader (reader/get (io/get-content-type (.toPath f)))]
            (Source. reader (InputSource. path) mtime))
          ; Hasn't changed since ltime
          nil))
      (throw (Exception. "Not a file")))))

(defn- get-url-source
  "Get a url source located at url or nil if it has not been updated
  since ltime."
  [url ltime]
  (let [conn (.openConnection url)]
    (if (not (nil? ltime))
      (.setIfModifiedSince conn ltime))
    (case (.getResponseCode conn)
      304 nil
      ;; TODO should handle additional response codes
      (let [ctype (.replaceFirst (.getContentType conn) ";.*" "")
            reader (reader/get ctype)
            ;; It's OK to do this. The stream will be closed by the
            ;; reader as part of end-of-parse cleanup.
            is (InputSource. (.getInputStream conn))
            mtime (.getLastModified conn)]
        (.setSystemId is (str url))
        (Source. reader is mtime)))))

(defn- get-source
  "Get the source identified by systemId or nil if it has not been updated
  since ltime where ltime is milliseconds since the epoch"
  [systemId ltime]
  (let [url (io/get-url systemId)]
    (case (.getProtocol url)
      "file" (get-file-source (.getPath url) ltime)
      (get-url-source url ltime))))

(defn- with-infoset-cache 
  "Cache source in memory as byte arrays of fastinfoset content"
  [cache systemId]
  (let [r (cache systemId)
        ctime (if (nil? r) nil (:ctime r))
        resource (get-source systemId ctime)]
    (if (nil? resource)
      cache
      (let [{:keys [reader inputSource mtime]} resource
            bos (ByteArrayOutputStream.)]
        (.setContentHandler reader ((:constructor serialize/infoset) bos))
        (.parse reader inputSource)
        (info "Caching infoset copy of" systemId)
        (assoc cache systemId (CachedSource. (.toByteArray bos) mtime))))))

(def ^{:private true} source-cache (atom {}))

(defn get
  "Get the source identified by systemId"
  [systemId & {:keys [cache] :or {cache true}}]
  (if cache
    (do
      (swap! source-cache with-infoset-cache systemId)
      (let [r (@source-cache systemId)
            is (InputSource. (ByteArrayInputStream. (:data r)))]
        (.setSystemId is systemId)
        (Source. (reader/get "application/fastinfoset") is (:ctime r))))
    (get-source systemId nil)))
