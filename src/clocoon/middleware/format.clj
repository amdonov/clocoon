(ns clocoon.middleware.format)

(def ^{:dynamic true} *default-format* "graphics")

(defn- parse-format [req]
  (let [cf ((req :cookies {}) "format" {:value *default-format*})]
    ((req :params {}) :format (:value cf))))

(defn- set-format [response f]
  (if (>= (response :status) 400)
    response
    (assoc response :cookies {"format" f})))

(defn wrap-format [app]
  (fn [req]
    (let [format (parse-format req)]
      (let [params (assoc (:params req) :format format)]
        (-> (app (assoc req :params params))
          (set-format format))))))

