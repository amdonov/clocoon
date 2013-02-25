(ns clocoon.reader
  (:refer-clojure :exclude [get])
  (:import (com.sun.xml.fastinfoset.sax SAXDocumentParser)
           (org.cyberneko.html.parsers SAXParser)
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

(defn get
  "Returns an XMLReader that's suitable for for the provided content type."
  [ctype]
  (case ctype
    "text/html" (get-html-reader)
    "application/xml" (get-xml-reader)
    "text/xml" (get-xml-reader)
    "application/fastinfoset" (get-infoset-reader)
    throw (Exception. (str "Unsupported content type" ctype))))

