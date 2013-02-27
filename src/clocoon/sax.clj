(ns clocoon.sax
  (:use [clojure.tools.logging :only (info error)]
        [clocoon.filter.core])
  (:require [clocoon.source :as source])
  (:import (clocoon.source Source)
           (javax.xml.transform TransformerFactory URIResolver)
           (javax.xml.transform.sax SAXSource SAXTransformerFactory)
           (org.xml.sax InputSource)
           (java.io File ByteArrayInputStream BufferedOutputStream
                    FileOutputStream ByteArrayOutputStream)
           (java.net URI)
           (java.nio.file Files Paths StandardOpenOption)
           (java.nio.charset Charset)
           (java.util Calendar UUID)))

(def ^{:dynamic true} *cachedir* "cache")

(defn get-path 
  [path & paths]
  (Paths/get path (into-array String paths)))

(defn make-cache-file []
  (let [cal (Calendar/getInstance)]
    (let [p 
          (get-path *cachedir*
                    (str (.get cal Calendar/YEAR))
                    (format "%02d" (+ 1 (.get cal Calendar/MONTH)))
                    (str (.get cal Calendar/DATE))
                    (str (.get cal Calendar/DATE))
                    (format "%02d" (.get cal Calendar/HOUR_OF_DAY))
                    (format "%02d" (.get cal Calendar/MINUTE))
                    (str (UUID/randomUUID)))]
      (.mkdirs (.toFile (.getParent p)))
      (.toFile p))))

(defn- rewrite-cache
  "An extra entry is added to the cache journal each time an entry is updated.
  This leads to multiple lines for each cacheId. This function rebuilds the
  cache journal to remove old entries."
  [cache]
  (info "Opening journal for rebuild")
  (with-open [file (Files/newBufferedWriter 
                     (get-path *cachedir* "journal")
                     (Charset/defaultCharset)
                     (into-array StandardOpenOption
                                 (list StandardOpenOption/WRITE
                                       StandardOpenOption/TRUNCATE_EXISTING
                                       StandardOpenOption/CREATE)))]
    (doall 
      (map (fn [field] 
             (.write file (str field "||" (get cache field) "\n"))) (keys cache)))))

(defn- build-cache []
  (let [file (.toFile (get-path *cachedir* "journal"))]
    (if (.exists file)
      (let [lines (seq (.split (slurp file) "\n"))
            fields (map (fn [line] (get (.split line "\\|\\|") 0)) lines)
            values (map (fn [line] (File. 
                                     (get (.split line "\\|\\|") 1))) lines)]
        (let [cache (zipmap fields values)]
          (rewrite-cache cache)
          cache))
      {})))

(def cache (build-cache))

(def journal (do 
               (info "Opening journal for logging")
               (.mkdirs (File. *cachedir*))
               (Files/newBufferedWriter
                 (get-path *cachedir* "journal")
                 (Charset/defaultCharset)
                 (into-array StandardOpenOption
                             (list StandardOpenOption/APPEND 
                                   StandardOpenOption/CREATE)))))

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
        file (make-cache-file)
        serializer (:constructor serializer)]
    (with-open [os (BufferedOutputStream. (FileOutputStream. file))]
      (apply parser (serializer os) filters))
    file))

(def ^{:private true} pipeline-cache (atom cache))

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
            (.write journal (str cacheId "||" file "\n"))
            (.flush journal)
            (assoc cache cacheId file)))
        cache))))

(defn pipeline [resource serializer & filters]
  (swap! pipeline-cache with-pipeline-cache resource serializer filters)
  (@pipeline-cache (get-pipeline-cache-id resource serializer filters)))
