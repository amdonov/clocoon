(ns clocoon.source)

(defprotocol PSource
  "The source for SAX events. Pairs an InputSource with an XMLReader and also includes a last-modified time to support caching."
  (reader [this] "Get the XMLReader")
  (input-source [this] "Get the InputSource")
  (last-modified [this] "Get the last-modified time in milliseconds since the epoch for the InputSource"))

;; Record implementation of the PSource protocol
(defrecord Source
  [r is mtime]
  PSource
  (reader [this]
    r)
  (input-source [this]
    is)
  (last-modified [this]
    mtime))

;; Map the Java ISource interface to the PSource protocol
(extend-type clocoon.ISource
    PSource
    (reader [this]
          (.getReader this))
    (input-source [this]
          (.getInputSource this))
    (last-modified [this]
       (.getLastModified this)))

