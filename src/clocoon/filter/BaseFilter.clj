(ns clocoon.filter.BaseFilter
  (:import (org.xml.sax InputSource)) 
  (:gen-class
    :main false
    :state state
    :init init
    :exposes-methods {setProperty ssetProperty
                      getProperty sgetProperty
                      parse sparse}
    :implements [org.xml.sax.ext.LexicalHandler]
    :extends org.xml.sax.helpers.XMLFilterImpl))

(def handler-prop "http://xml.org/sax/properties/lexical-handler")

(defn -init []
  [[] (atom {:lexHandler nil})])

(defn set-field [this key value]
  (swap! (.state this) into {key value}))

(defn get-field [this key]
  (@(.state this) key))

(defn pass-event [this f & args]
  (let [handler (get-field this :lexHandler)]
    (if (not (nil? handler))
      (apply f handler args))))

(defn -comment [this ch start length]
  (pass-event #(.comment %1 %2 %3 %4)))

(defn -endCDATA [this]
  (pass-event this #(.endCDATA %)))

(defn -endDTD [this]
  (pass-event this #(.endDTD %)))

(defn -endEntity [this name]
  (pass-event this #(.endEntity %)))

(defn -startCDATA [this]
  (pass-event this #(.startCDATA %)))

(defn -startDTD [this name publicId systemId]
  (pass-event this #(.startDTD %1 %2 %3 %4) name publicId systemId))

(defn -startEntity [this name]
  (pass-event this #(.startEntity %1 %2) name))

(defn -parse [this ^InputSource input]
  (let [parent (.getParent this)]
    (if (not (nil? parent))
      (.setProperty parent handler-prop this)))
  (.sparse this input))

(defn -setProperty [this name value]
  (if (= handler-prop name)
    (set-field this :lexHandler value)
    (.ssetProperty this name value)))

(defn -getProperty [this name]
  (if (= handler-prop name)
    (get-field this :lexHandler)
    (.sgetProperty this name)))
