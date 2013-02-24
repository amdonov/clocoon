(ns clocoon.source
  (:require [clocoon.io :as io])
  (:import (com.sun.xml.fastinfoset.sax SAXDocumentParser)
           (java.io File)
           (org.cyberneko.html.parsers SAXParser)
           (org.xml.sax InputSource)
           (org.xml.sax.helpers XMLReaderFactory))) 

(defn- get-xml-reader
  "Get a new XMLReader suitable for parsing XML"
  []
  (XMLReaderFactory/createXMLReader))

(defn- get-html-reader 
  "Get a new XMLReader suitable for parsing HTML"
  []
  (let [parser (SAXParser.)]
    (.setFeature parser 
                 "http://cyberneko.org/html/features/insert-namespaces" true)
    (.setProperty parser 
                  "http://cyberneko.org/html/properties/names/elems" "lower")
    parser))

(defn- get-infoset-reader 
  "Get a new XMLReader suitable for parsing Fast Infoset" 
  []
  (SAXDocumentParser.))

(defn get-reader
  "Returns an XMLReader that's suitable for for the provided content type."
  [ctype]
  (case ctype
    "text/html" (get-html-reader)
    "application/xml" (get-xml-reader)
    "text/xml" (get-xml-reader)
    "application/fastinfoset" (get-infoset-reader)
    throw (Exception. (str "Unsupported content type" ctype))))

(defrecord Source 
  [reader inputSource mtime])

(defn- get-file-source
  "Get a file source located at path or nil if it has not been updated
  since ltime. Throws exceptions if the file does not exist or is in a
  format that cannot be parsed."
  [path ltime]
  (let [f (File. path)]
    (if (and (.exists f) (.isFile f))
      (let [mtime (.lastModified f)]
        (if (or (nil? ltime) (> mtime ltime)) 
          ; Either ltime is nil or the file has changed
          (let [reader (get-reader (io/get-content-type (.toPath f)))]
            (Source. reader (InputSource. path) mtime))
          ; Hasn't changed since ltime
          nil))
      (throw (Exception. "Not a file")))))

(defn- get-url-source
  "Get a url source located at url or nil if it has not been updated
  since ltime."
  [url ltime]
  (let [conn (.openConnection url)]
    (if (not (nil? ltime))
      (.setIfModifiedSince conn ltime))
    (case (.getResponseCode conn)
      304 nil
      ;; TODO should handle additional response codes
      (let [ctype (.replaceFirst (.getContentType conn) ";.*" "")
            reader (get-reader ctype)
            ;; It's OK to do this. The stream will be closed by the
            ;; reader as part of end-of-parse cleanup.
            is (InputSource. (.getInputStream conn))
            mtime (.getLastModified conn)]
        (.setSystemId is (str url))
        (Source. reader is mtime)))))

(defn get-source 
  "Get the source identified by systemId or nil if it has not been updated
  since ltime where ltime is milliseconds since the epoch"
  [systemId ltime]
  (let [url (io/get-url systemId)]
    (case (.getProtocol url)
      "file" (get-file-source (.getPath url) ltime)
      (get-url-source url ltime))))
