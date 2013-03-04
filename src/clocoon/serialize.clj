(ns clocoon.serialize
  (:use [clocoon.core])
  (:import (javax.xml.transform.sax SAXTransformerFactory)
           (javax.xml.transform.stream StreamResult)
           (javax.xml.transform.dom DOMResult)
           (org.xml.sax ContentHandler)
           (org.xhtmlrenderer.pdf ITextRenderer)
           (clocoon ISerializer)
           (com.sun.xml.fastinfoset.sax SAXDocumentSerializer)))

(defprotocol PSerializer
  (create [this os])
  (content-type [this]))

(extend-type ISerializer
    PSerializer
    (create [this os]
          (.create this os))
    (content-type [this]
          (.getContentType this)))

(defrecord Serializer [factory ctype cid]
  PSerializer
  (create [this os]
    (factory os))
  (content-type [this]
    ctype)
  PCacheable
  (cache-valid? [this ctime]
    true)
  (cache-id [this]
    cid))

(defn- create-stream-serializer
  "Get a ContentHandler for streaming SAX events as XML/HTML/text 
  content to the provided OutputStream"
  [os]
  (let [serializer (.. (SAXTransformerFactory/newInstance) 
                     (newTransformerHandler))]
    (.setResult serializer (StreamResult. os))
    serializer))

(defn- create-dom-serializer
  "A helper method for serializers that cannot directly deal with SAX
  events and must have access to the completed DOM. It builds up the 
  DOM and passes it to the provided callback"
  [callback]
  (let [serializer (.. (SAXTransformerFactory/newInstance)
                     (newTransformerHandler))
        result (DOMResult.)
        proxy (reify ContentHandler
                (characters [this ch start length]
                  (.characters serializer ch start length))
                (endElement [this uri localName qName]
                  (.endElement serializer uri localName qName))
                (endPrefixMapping [this prefix]
                  (.endPrefixMapping serializer prefix))
                (processingInstruction [this target data]
                  (.processingInstruction serializer target data))
                (setDocumentLocator [this locator]
                  (.setDocumentLocator serializer locator))
                (skippedEntity [this name]
                  (.skippedEntity serializer name))
                (startDocument [this]
                  (.startDocument serializer))
                (startElement [this uri localName qName atts]
                  (.startElement serializer uri localName qName atts))
                (endDocument [this]
                  (.endDocument serializer)
                  (callback (.getNode result))))]
    (.setResult serializer result)
    proxy))

(defn- create-pdf-serializer
  "Create a serializer that uses XHTMLRender to produce a PDF from XHTML"
  [os]
  (create-dom-serializer (fn [dom]
                           (let [renderer (ITextRenderer.)]
                             (.setDocument renderer dom "")
                             (.layout renderer)
                             (.createPDF renderer os)))))

(defn- create-infoset-serializer
  "Get a ContentHandler for streaming SAX events as Fast Infoset to the
  provided OutputStream"
  [os]
  (let [serializer (SAXDocumentSerializer.)]
    (.setOutputStream serializer os)
    serializer))

(def infoset (Serializer. create-infoset-serializer 
                          "application/fastinfoset" "fis"))

(def pdf (Serializer. create-pdf-serializer 
                      "application/pdf" "pdf"))

(def html (Serializer. create-stream-serializer "text/html" "html"))

(def xml (Serializer. create-stream-serializer "text/xml" "xml"))
