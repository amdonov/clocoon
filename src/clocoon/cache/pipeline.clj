(ns clocoon.cache.pipeline
  (:use [clojure.tools.logging :only (info error)]
        [clocoon.cache.core]
        [clocoon.filter.core])
  (:require [clocoon.serializer :as serializer]
            [clocoon.sax :as sax]
            [clocoon.cache.resource])
  (:import 
    (java.io File BufferedOutputStream FileOutputStream)
    (java.nio.file Files Paths StandardOpenOption)
    (java.nio.charset Charset)
    (java.util Calendar UUID)))

(def ^{:dynamic true} *cachedir* "cache")
(def journal)
(def pipeline-cache)

;; Create a placeholder for a cache free pipeline
;; It won't work if they don't elect to use a cache
(intern 'clocoon.sax 
        'pipeline-nocache 
        (fn [& args] (throw
                       (Exception. 
                         "Please call use-file-cache from an init method."))))

(defn- get-path 
  [path & paths]
  (Paths/get path (into-array String paths)))

(defn- make-cache-file []
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
        (let [file (make-cache-file)]
          (with-open [os (BufferedOutputStream. (FileOutputStream. file))]
            (sax/do-pipeline resource (serializer/handler serializer os) filters))
          (.write journal (str cache-id "||" file "\n"))
          (.flush journal)
          (assoc cache cache-id file)))
      cache)))

(defn- wrap
  [f]
  ; Bind pipeline-nocache to the orginial function version
  (intern 'clocoon.sax 'pipeline-nocache f)
  (fn [resource serializer & filters]
    (swap! pipeline-cache with-pipeline-cache resource serializer filters)
    {:body (@pipeline-cache (get-pipeline-cache-id resource 
                                                   serializer filters))
     :headers {"Content-Type" (serializer/content-type serializer)}}))

(defn use-file-cache []
  (def ^{:private true} pipeline-cache (atom (build-cache)))
  (def journal (do 
                 (info "Opening journal for logging")
                 (.mkdirs (File. *cachedir*))
                 (Files/newBufferedWriter
                   (get-path *cachedir* "journal")
                   (Charset/defaultCharset)
                   (into-array StandardOpenOption
                               (list StandardOpenOption/APPEND 
                                     StandardOpenOption/CREATE)))))
  (alter-var-root #'clocoon.sax/pipeline wrap))
