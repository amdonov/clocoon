(ns clocoon.filter.core
  (:import (org.xml.sax XMLFilter)))

(defprotocol Filter
  (get-filter [this]))

(extend-type XMLFilter
  Filter
  (get-filter [this] this))
