;; This code borrows heavily from ring's file_info middleware but also
;; use's etags and doesn't guess the content-type
;; (https://github.com/mmcgrana/ring/blob/master/ring-core/src/ring/middleware/file_info.clj)
(ns clocoon.middleware.modified
  (:require [ring.util.response :as res])
  (:import java.io.File
           (java.util Date Locale TimeZone)
           java.text.SimpleDateFormat))

(defn- ^SimpleDateFormat make-http-format
  "Formats or parses dates into HTTP date format (RFC 822/1123)."
  []
  ;; SimpleDateFormat is not threadsafe, so return a new instance each time
  (doto (SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss ZZZ" Locale/US)
    (.setTimeZone (TimeZone/getTimeZone "UTC"))))

(defn- not-modified-since?
  "Has the file been modified since the last request from the client?"
  [{headers :headers :as req} last-modified etag]
  (let [modified-since (headers "if-modified-since")
           old-tag (headers "if-none-match")]
    (if (and (not (nil? old-tag)) (= old-tag etag))
      true
      (and (not (nil? modified-since))
        (not (.before (.parse (make-http-format) modified-since)
                      last-modified))))))

(defn- file-info-response
  [{:keys [body] :as response} req]
  (if (instance? File body)
    (let [file-length (.length ^File body)
          lmodified   (Date. (.lastModified ^File body))
          etag (.getName body)
          response    (-> response
                        (res/header "ETag" etag)
                        (res/header
                          "Last-Modified"
                          (.format (make-http-format) lmodified)))]
      (if (not-modified-since? req lmodified etag)
        (-> response (res/status 304)
          (res/header "Content-Length" 0)
          (assoc :body ""))
        (-> response (res/header "Content-Length" file-length))))
    response))

(defn wrap-modified
  [app]
  (fn [req]
    (-> (app req)
      (file-info-response req))))

