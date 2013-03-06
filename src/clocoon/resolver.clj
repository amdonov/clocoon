(ns clocoon.resolver
  (:refer-clojure :exclude [resolve])
  (:import (java.io File)))

(defn resolve
  "Resolve a String systemId to another resource type. Could be used to implement custom protocols such as those allowed by Cocoon"
  [systemId]
  (let [url (.toURL (.resolve (.toURI (File. "")) systemId))]
    (case (.getProtocol url)
      "file" (File. (.getPath url))
      url)))
