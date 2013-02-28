(ns clocoon.filter.xsl
  (:refer-clojure :exclude [filter])
  (:use [clojure.tools.logging :only (info error)]
        [clocoon.core]
        [clocoon.filter.core])
  (:require [clocoon.source :as source])
  (:import (javax.xml.transform TransformerFactory URIResolver)
           (javax.xml.transform.sax SAXSource SAXTransformerFactory)
           (java.net URI)))

(defrecord XSLFilter [xmlfilter mtime cid]
  PCacheable
  (cache-id [this]
    cid)
  (cache-valid? [this ctime]
    (> ctime mtime))
  PFilter
  (get-filter [this]
    xmlfilter))

(defn- get-source [systemId]
  (let [{:keys [reader inputSource mtime]} (source/fetch systemId)]
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
  (cache-valid? systemId ctime))

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

(defn- get-xsl-filter [systemId params]
  (let [templates (get-cached-templates systemId)]
    (let [f (.. (SAXTransformerFactory/newInstance) 
              (newXMLFilter (:obj templates)))
          ctime (:ctime templates)]
      (let [t (.getTransformer f)]
        (if (> 0 (count params))
          (apply (fn [k] (.setParameter t k (params k)))
                 (keys params))))
      (XSLFilter. f ctime (str systemId params)))))

(defn filter [systemId]
  (fn 
    ([]
     (get-xsl-filter systemId {}))
    ([params]
     (get-xsl-filter systemId params))))

