(ns clocoon.io
  (:import (java.io File)
           (java.util Calendar UUID)
           (java.nio.file Files Paths)))

(def ^{:dynamic true} *cachedir* "cache")

(defn get-url [path]
  (let [cwd (.toURI (File. ""))]
    (.toURL (.resolve cwd path))))

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

(defn get-content-type [path]
  (Files/probeContentType path))
