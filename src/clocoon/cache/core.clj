(ns clocoon.cache.core)

(defprotocol PCacheable
  (cache-valid? [this ctime])
  (cache-id [this]))

(extend-type clocoon.ICacheable
    PCacheable
    (cache-valid? [this ctime]
          (.isCacheValid this ctime))
    (cache-id [this]
          (.getCacheId this)))
