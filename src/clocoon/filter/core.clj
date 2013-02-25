(ns clocoon.filter.core
  (:import (org.xml.sax XMLFilter)))

(defprotocol Filter
  (get-filter [this]))

(defprotocol CachedFilter
  (cache-valid? [this ctime])
  (cache-id [this]))

(extend-type XMLFilter
  Filter
  (get-filter [this] this))
