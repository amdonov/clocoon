(ns clocoon.sax
  (:use [clojure.tools.logging :only (info error)]
        [clocoon.core]
        [clocoon.filter.core])
  (:require [clocoon.source :as source]
            [clocoon.serialize :as serialize])
  (:import (clocoon.source Source)
           (javax.xml.transform TransformerFactory URIResolver)
           (javax.xml.transform.sax SAXSource SAXTransformerFactory)
           (org.xml.sax InputSource)
           (org.xml.sax.ext LexicalHandler)
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
  This leads to multiple lines for each cache-id. This function rebuilds the
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

(defn init []
  (def ^{:private true} pipeline-cache (atom (build-cache)))
  (def journal (do 
                 (info "Opening journal for logging")
                 (.mkdirs (File. *cachedir*))
                 (Files/newBufferedWriter
                   (get-path *cachedir* "journal")
                   (Charset/defaultCharset)
                   (into-array StandardOpenOption
                               (list StandardOpenOption/APPEND 
                                     StandardOpenOption/CREATE))))))

(defn- wrap-reader 
  "Wrap an XMLReader with an XMLFilter"
  [reader xmlfilter]
  (let [f (get-filter xmlfilter)]
    (.setParent f reader)
    f))

(defn get-parser
  [resource]
  (let [res (source/fetch resource)
        reader (source/reader res)
        inputSource (source/input-source res)]
    (fn [handler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader handler)
        (if (instance? LexicalHandler handler)
          (.setProperty reader "http://xml.org/sax/properties/lexical-handler"
                        handler))
        (.parse reader inputSource)))))

(defn- do-pipeline [resource serializer filters]
  (let [parser (get-parser resource)
        file (make-cache-file)]
    (with-open [os (BufferedOutputStream. (FileOutputStream. file))]
      (apply parser (serialize/create serializer os) filters))
    file))

(defn- cached-filter-valid? [ctime f]
  (if (satisfies? PCacheable f)
    (cache-valid? f ctime)
    true))

(defn- cached-pipeline-valid? [ctime resource filters]
  (and 
    (cache-valid? resource ctime)
    (every? (partial cached-filter-valid? ctime) filters)))

(defn- get-pipeline-cache-id [resource serializer filters]
  (str (cache-id resource) (cache-id serializer) (reduce str (map cache-id (filter (partial satisfies? PCacheable) filters)))))

(defn- with-pipeline-cache [cache resource serializer filters]
  (let [cache-id (get-pipeline-cache-id resource serializer filters)
        f (cache cache-id)]
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
          (.write journal (str cache-id "||" file "\n"))
          (.flush journal)
          (assoc cache cache-id file)))
      cache)))

(defn pipeline [resource serializer & opts+filters]
  (swap! pipeline-cache with-pipeline-cache resource serializer opts+filters)
  {:body (@pipeline-cache (get-pipeline-cache-id resource 
                                                 serializer opts+filters))
   :headers {"Content-Type" (serialize/content-type serializer)}})
