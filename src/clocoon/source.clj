(ns clocoon.source)

(defprotocol PSource
  (reader [this])
  (input-source [this])
  (last-modified [this]))

(defrecord Source
  [r is mtime]
  PSource
  (reader [this]
    r)
  (input-source [this]
    is)
  (last-modified [this]
    mtime))

(extend-type clocoon.ISource
    PSource
    (reader [this]
          (.getReader this))
    (input-source [this]
          (.getInputSource this))
    (last-modified [this]
       (.getLastModified this)))

