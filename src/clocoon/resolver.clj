(ns clocoon.resolver
  (:refer-clojure :exclude [resolve])
  (:import (java.io File)))

(defn resolve
  [systemId]
  (let [url (.toURL (.resolve (.toURI (File. "")) systemId))]
    (case (.getProtocol url)
      "file" (File. (.getPath url))
      url)))
