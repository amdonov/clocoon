(ns clocoon.filter.core
  (:import (org.xml.sax XMLFilter)))

(defprotocol PFilter
  "Sits between the Source and Serializer to alter SAX Events or produce side effects"
  (get-filter [this] "Get a fresh XMLFilter for use in a pipeline. The filter may also implement LexicalHandler."))

(extend-type XMLFilter
  PFilter
  (get-filter [this] this))

(extend-type clocoon.IFilter
     PFilter
     (get-filter [this]
           (.getFilter this)))

