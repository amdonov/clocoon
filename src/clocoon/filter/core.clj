(ns clocoon.filter.core
  (:import (org.xml.sax XMLFilter)))

(defprotocol PFilter
  (get-filter [this]))

(extend-type XMLFilter
  PFilter
  (get-filter [this] this))
