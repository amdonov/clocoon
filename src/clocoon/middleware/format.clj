(ns clocoon.middleware.format
  (:require [ring.util.response :as response]))

(def ^{:dynamic true} *default-format* "graphics")

(defn- parse-format
  "Get the current value for format in the following order of preference 1) request parameter, 2) cookie value 3) default value."
  [req]
  (let [cf ((req :cookies {}) "format" {:value *default-format*})]
    ((req :params {}) :format (:value cf))))

(defn- set-format
  "Set a cookie with format value"
  [response f]
  (if (>= (response :status) 400)
    response; Did this to address Google Chrome's request for favicon.ico
    ;; TODO Add an expiration time for the cookie
    (response/set-cookie response "format" f)))

(defn wrap-format
  "Wrap the handler with the added format functionality"
  [app]
  (fn [req]
    (let [format (parse-format req)
          params (assoc (:params req) :format format)]
      (-> (app (assoc req :params params))
        (set-format format)))))

