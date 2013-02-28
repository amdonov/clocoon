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
  (sax/pipeline source serialize/html (html-transform {"format" f})))

(defn- just-xml [source]
  (sax/pipeline source serialize/xml))

(defn- basic-pdf [source f]
  (sax/pipeline source serialize/pdf (html-transform {"format" f})))

(defn- source [src]
  (str "sample/" src ".xml"))

(defroutes app-routes
           (GET "/" [] "Welcome")
           (GET "/product/:src.html"
                [src format] {:body (basic-html (source src) format)
                              :headers {"Content-Type" "text/html"}})
           (GET "/product/:src.xml"
                [src] {:body (just-xml (source src))
                              :headers {"Content-Type" "text/xml"}})
           (GET "/product/:src.pdf"
                [src format] {:body (basic-pdf (source src) format)
                              :headers {"Content-Type" "application/pdf"}})
           (route/not-found "Not Found"))

(def app (handler/site (-> app-routes
                         wrap-format 
                         wrap-modified)))

