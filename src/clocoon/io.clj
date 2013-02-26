(ns clocoon.io
  (:use [clojure.tools.logging :only (info error)])
  (:import (java.io File)
           (java.util Calendar UUID)
           (java.nio.file Files Paths StandardOpenOption)
           (java.nio.charset Charset)))

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

