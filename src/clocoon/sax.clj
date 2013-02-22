(ns clocoon.sax
  (:use [clojure.tools.logging :only (info error)])
  (:require [clocoon.io :as io])
  (:import (javax.xml.transform TransformerFactory URIResolver)
           (javax.xml.transform.sax SAXSource SAXTransformerFactory)
           (javax.xml.transform.stream StreamResult)
           (javax.xml.transform.dom DOMResult)
           (org.xml.sax InputSource ContentHandler)
           (java.io File ByteArrayInputStream BufferedOutputStream
                    FileOutputStream ByteArrayOutputStream)
           (java.net URI)
           (org.xhtmlrenderer.pdf ITextRenderer)
           (org.xml.sax.helpers XMLReaderFactory)
           (org.cyberneko.html.parsers SAXParser)
           (com.sun.xml.fastinfoset.sax SAXDocumentParser 
                                        SAXDocumentSerializer)))

(defprotocol CachedFilter
  (cache-valid? [this ctime])
  (cache-id [this]))

(defrecord HandlerFactory [constructor cacheId])

(defrecord XSLFilter [xmlfilter mtime cacheId]
  CachedFilter
  (cache-id [this]
    (:cacheId this))
  (cache-valid? [this ctime]
    (> ctime mtime)))

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

(defn- wrap-reader 
  "Wrap an XMLReader with an XMLFilter"
  [reader xmlfilter]
  (let [f (if (instance? XSLFilter xmlfilter)
            (:xmlfilter xmlfilter)
            xmlfilter)]
    (.setParent f reader)
    f))

(defn- create-stream-handler
  "Get a ContentHandler for streaming SAX events as XML/HTML/text content to the
  provided OutputStream"
  [os]
  (let [handler (.. (SAXTransformerFactory/newInstance) 
                  (newTransformerHandler))]
    (.setResult handler (StreamResult. os))
    handler))

(defn create-dom-handler
  [os callback]
  (let [handler (.. (SAXTransformerFactory/newInstance)
                  (newTransformerHandler))
        result (DOMResult.)
        proxy (reify ContentHandler 
                (characters [this ch start length]
                  (.characters handler ch start length))
                (endElement [this uri localName qName]
                  (.endElement handler uri localName qName))
                (endPrefixMapping [this prefix]
                  (.endPrefixMapping handler prefix))
                (processingInstruction [this target data]
                  (.processingInstruction handler target data))
                (setDocumentLocator [this locator]
                  (.setDocumentLocator handler locator))
                (skippedEntity [this name]
                  (.skippedEntity handler name))
                (startDocument [this]
                  (.startDocument handler))
                (startElement [this uri localName qName atts]
                  (.startElement handler uri localName qName atts))
                (endDocument [this]
                  (.endDocument handler)
                  (callback (.getNode result))))]
    (.setResult handler result)
    proxy))

(defn- create-pdf-handler
  [os]
  (create-dom-handler os (fn [dom]
                           (let [renderer (ITextRenderer.)]
                             (.setDocument renderer dom "")
                             (.layout renderer)
                             (.createPDF renderer os)))))

(defn- create-infoset-handler
  "Get a ContentHandler for streaming SAX events as Fast Infoset to the
  provided OutputStream"
  [os]
  (let [handler (SAXDocumentSerializer.)]
    (.setOutputStream handler os)
    handler))

(def infoset-handler (HandlerFactory. create-infoset-handler "fis"))

(def pdf-handler (HandlerFactory. create-pdf-handler "pdf"))

(def stream-handler (HandlerFactory. create-stream-handler ""))

(defrecord Resource [reader inputSource mtime])

(defn- get-file-resource
  "Get a file resource located at path or nil if it has not been updated
  since ltime. Throws exceptions if the file does not exist or is in a
  format that cannot be parsed."
  [path ltime]
  (let [f (File. path)]
    (if (and (.exists f) (.isFile f))
      (let [mtime (.lastModified f)]
        (if (or (nil? ltime) (> mtime ltime)) 
          ; Either ltime is nil or the file has changed
          (let [reader
                (case (io/get-content-type (.toPath f))
                  "text/html" (get-html-reader)
                  "application/xml" (get-xml-reader) 
                  (throw (Exception. "Unsupported file format")))]
            (Resource. reader (InputSource. path) mtime))
          ; Hasn't changed since ltime
          nil))
      (throw (Exception. "Not a file")))))

(defn- get-url-resource
  "Get a url resource located at url or nil if it has not been updated
  since ltime. Throws exceptions if the file does not exist or is in a
  format that cannot be parsed."
  [url ltime]
  (let [conn (.openConnection url)]
    (if (not (nil? ltime))
      (.setIfModifiedSince conn ltime))
    (case (.getResponseCode conn)
      304 nil
      ;; TODO should handle additional response codes
      (let [ctype (.replaceFirst (.getContentType conn) ";.*" "")
            reader (case ctype
                     "text/html" (get-html-reader)
                     "application/xml" (get-xml-reader)
                     "text/xml" (get-xml-reader)
                     (throw (Exception. 
                              (str "Unsupported content type" ctype))))
            is (InputSource. (.getInputStream conn))
            mtime (.getLastModified conn)]
        (.setSystemId is (str url))
        (Resource. reader is mtime)))))

(defn- get-resource 
  "Get the resource identified by systemId or nil if it has not been updated
  since ltime where ltime is milliseconds since the epoch"
  [systemId ltime]
  (let [url (io/get-url systemId)]
    (case (.getProtocol url)
      "file" (get-file-resource (.getPath url) ltime)
      (get-url-resource url ltime))))

(defrecord CachedResource [data mtime])

(def ^{:private true} resource-cache (atom {}))

(defn- with-infoset-cache [cache systemId]
  (let [r (cache systemId)]
    (let [ltime (if (nil? r) nil (:mtime r))]
      (let [resource (get-resource systemId ltime)]
        (if (nil? resource) 
          cache
          (let [{:keys [reader inputSource mtime]} resource 
                bos (ByteArrayOutputStream.)]
            (.setContentHandler reader (create-infoset-handler bos))
            (.parse reader inputSource)
            (info "Caching infoset copy of" systemId)
            (assoc cache systemId (CachedResource. 
                                    (.toByteArray bos) mtime))))))))

(defn- get-cached-resource [systemId]
  (swap! resource-cache with-infoset-cache systemId)
  (let [r (@resource-cache systemId)]
    (let [is (InputSource. (ByteArrayInputStream. (:data r)))]
      (.setSystemId is systemId)
      (Resource. (get-infoset-reader) is (:mtime r)))))

(defn get-parser
  [systemId]
  (let [{:keys [reader inputSource]} (get-cached-resource systemId)]
    (fn [contentHandler & filters]
      (let [reader (reduce wrap-reader reader filters)]
        (.setContentHandler reader contentHandler)
        (.parse reader inputSource)))))

(defn get-source [systemId]
  (let [{:keys [reader inputSource mtime]} (get-cached-resource systemId)]
    {:source (SAXSource. reader inputSource)
     :mtime mtime}))

(defrecord Template [obj deps ctime])

(def ^{:private true} templates-cache (atom {}))

(defn- get-templates [systemId]
  (let [tf (TransformerFactory/newInstance)]
    (with-local-vars [deps #{systemId} ctime 0]
      (.setURIResolver tf (reify 
                            URIResolver
                            (resolve [this href, base]
                              (let [systemId (str (.resolve (URI. base) href))
                                    source (get-source systemId)]
                                (var-set deps (conj (var-get deps) systemId))
                                (var-set ctime (max (:mtime source)
                                                    (var-get ctime)))
                                (:source source)))))
      (let [source (get-source systemId)] 
        (Template. (.newTemplates tf (:source source))
                   (var-get deps)
                   (max (var-get ctime) (:mtime source)))))))

(defn- cached-resource-valid? [ctime systemId]
  (let [mtime (:mtime (get-cached-resource systemId))]
    (<= mtime ctime)))

(defn- cached-template-valid? [template]
  (let [{:keys [ctime deps]} template]
    (every? (partial cached-resource-valid? ctime) deps)))

(defn- with-templates-cache [cache systemId]
  (let [t (cache systemId)]
    (if (or (nil? t) (not (cached-template-valid? t)))
      (do
        (info "Caching templates for" systemId)
        (assoc cache systemId (get-templates systemId)))
      cache)))

(defn- get-cached-templates [systemId]
  (swap! templates-cache with-templates-cache systemId)
  (@templates-cache systemId))

(defn- get-xsl-filter-internal [systemId params]
  (let [templates (get-cached-templates systemId)]
    (let [f (.. (SAXTransformerFactory/newInstance) 
              (newXMLFilter (:obj templates)))
          ctime (:ctime templates)]
      (let [t (.getTransformer f)]
        (if (> 0 (count params))
          (apply (fn [k] (.setParameter t k (params k)))
                 (keys params))))
      (XSLFilter. f ctime (str systemId params)))))

(defn get-xsl-filter [systemId]
  (fn 
    ([]
     (get-xsl-filter-internal systemId {}))
    ([params]
     (get-xsl-filter-internal systemId params))))

(defn- do-pipeline [systemId handler-factory filters]
  (let [parser (get-parser systemId)
        file (io/make-cache-file)
        handler (:constructor handler-factory) ]
    (with-open [os (BufferedOutputStream. (FileOutputStream. file))]
      (apply parser (handler os) filters))
    file))

(def ^{:private true} pipeline-cache (atom {}))

(defn- cached-filter-valid? [ctime f]
  (if (satisfies? CachedFilter f)
    (cache-valid? f ctime)
    true))

(defn- cached-pipeline-valid? [ctime systemId filters]
  (and 
    (cached-resource-valid? ctime systemId)
    (every? (partial cached-filter-valid? ctime) filters)))

(defn- get-pipeline-cache-id [systemId handler-factory filters]
  (str systemId (:cacheId handler-factory) (reduce str (map :cacheId (filter (partial satisfies? CachedFilter) filters)))))

(defn- with-pipeline-cache [cache systemId handler-factory filters]
  (let [cacheId (get-pipeline-cache-id systemId handler-factory filters)]
    (let [f (cache cacheId)]
      (if (or (nil? f)
              (not (.exists f))
              (not (cached-pipeline-valid? 
                     (.lastModified f) 
                     systemId
                     filters)))
        (do 
          (if (not (nil? f))
            (.delete f))
          (assoc cache cacheId (do-pipeline systemId handler-factory filters)))
        cache))))

(defn pipeline [systemId handler-factory & filters]
  (swap! pipeline-cache with-pipeline-cache systemId handler-factory filters)
  (@pipeline-cache (get-pipeline-cache-id systemId handler-factory filters)))
