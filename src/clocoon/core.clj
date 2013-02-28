(ns clocoon.core)

(defprotocol PCacheable
  (cache-valid? [this ctime])
  (cache-id [this]))
