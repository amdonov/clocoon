(ns clocoon.cache.core)

(defprotocol PCacheable
  "A PResource, PFilter, or PSerializer that supports caching"
  (cache-valid? [this ctime] "Has this item changed since the ctime, the time in milliseconds since the epoch that or is the cache still valid?")
  (cache-id [this] "Contribution of this item to the pipeline's cache-id. Need not be unique, but it should differentiate from other components."))

(extend-type clocoon.ICacheable
    PCacheable
    (cache-valid? [this ctime]
          (.isCacheValid this ctime))
    (cache-id [this]
          (.getCacheId this)))
