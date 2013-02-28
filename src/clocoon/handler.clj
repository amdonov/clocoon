(ns clocoon.handler
  (:use compojure.core
        clocoon.middleware.modified
        clocoon.middleware.format)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clocoon.sax :as sax]
            [clocoon.filter.xsl :as xsl]
            [clocoon.serialize :as serialize]))

(def html-transform (xsl/filter "sample/sample.xsl"))

(defn- basic-html [source f]
  (sax/pipeline source serialize/html (html-transform)))

(defn- just-xml [source]
  (sax/pipeline source serialize/xml))

(defn- basic-pdf [source f]
  (sax/pipeline source serialize/pdf (html-transform "format" f)))

(defn- source [src]
  (str "sample/" src ".xml"))

(defroutes product-routes
           (GET "/:src.html"
                [src format] (basic-html (source src) format))
           (GET "/:src.xml"
                [src] (just-xml (source src)))
           (GET "/:src.pdf"
                [src format] (basic-pdf (source src) format)))

(defroutes app-routes
           (GET "/" [] "Welcome")
           (context "/product" [] product-routes)
           (route/not-found "Not Found"))

(def app (handler/site (-> app-routes
                         wrap-format 
                         wrap-modified)))

