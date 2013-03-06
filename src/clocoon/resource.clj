(ns clocoon.resource
  (:require [clocoon.reader :as reader]
            [clocoon.resolver :as resolver]
            [clocoon.cache.core :as cache]
            [clocoon.source])
  (:import (clocoon.source Source) 
           (java.io File)
           (java.net URL)
           (java.nio.file Files)
           (org.xml.sax InputSource)))

(defprotocol PResource
  "A reference to a PSource. Could be a String, File, URL, or something else. This is the start of pipeline to avoid the overhead of creating a PSource in the eent that a pipeline is cached and need not be rerun."
  (fetch [this] [this mtime] "Return the PSource associated with this PResource or nil if it hasn't been modified since mtime"))

(extend-protocol PResource
  clocoon.IResource
  (fetch 
    ([this]
     (.fetch this))
    ([this mtime]
     (.fetch this mtime)))
  File
  (fetch
    ([this]
     {:pre [(.isFile this)]}
     (let [reader (reader/get (Files/probeContentType (.toPath this)))
           mtime (.lastModified this)]
       (Source. reader (InputSource. (str this)) mtime)))
    ([this mtime]
     (if-not (cache/cache-valid? this mtime)
       (fetch this)
       nil)))
  URL
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
           (Source. reader is mtime))))))
  String
  (fetch
    ([this]
     (fetch (resolver/resolve this)))
    ([this mtime]
     (fetch (resolver/resolve this) mtime))))
