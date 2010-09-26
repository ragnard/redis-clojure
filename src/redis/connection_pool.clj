(ns redis.connection-pool
  (:use [redis.connection :only (RedisConnectionPool make-connection close get-server-spec)])
  (:import [org.apache.commons.pool.impl GenericKeyedObjectPool]
           [org.apache.commons.pool KeyedPoolableObjectFactory]))

;; Connection pooling implemented using Apache commons-pool.

(defrecord ConnectionPool [#^GenericKeyedObjectPool object-pool]
  RedisConnectionPool
  (get-connection [this server-spec]
    (.borrowObject object-pool server-spec))
  (release-connection [this connection]
    (let [server-spec (get-server-spec connection)]                  
      (.returnObject object-pool server-spec connection)))
  (release-connection [this connection exception]
    (let [server-spec (get-server-spec connection)]                  
      (.invalidateObject object-pool server-spec connection))))

(defn- make-connection-factory []
  (reify KeyedPoolableObjectFactory
    (makeObject [this key] (make-connection key))
    (activateObject [this key obj])
    (validateObject [this key obj] true) ; TODO: PING? IsConnected?
    (passivateObject [this key obj])
    (destroyObject [this key obj] (close obj))))

(defn- set-pool-option [#^GenericKeyedObjectPool pool [opt v]]
  (case opt
    :max-active (.setMaxActive pool v)
    :max-total (.setMaxTotal pool v)
    :max-idle (.setMaxIdle pool v)
    :when-exhausted-action (.setWhenExhaustedAction pool v)
    :test-on-borrow (.setTestOnBorrow pool v)
    :test-on-return (.setTestOnReturn pool v)
    :time-between-eviction-runs-ms (.setTimeBetweenEvictionRunsMillis pool v)
    :min-evictable-idle-time-ms (.setMinEvictableIdleTimeMillis pool v)
    :test-while-idle (.setTestWhileIdle pool v)
    :min-idle (.setMinIdle pool v)
    :lifo (.setLifo pool v))
  pool)

(defn-  make-generic-keyed-object-pool [options]
  (let [factory (make-connection-factory)
        pool (GenericKeyedObjectPool. factory)]
    (reduce set-pool-option pool options)))

(defn make-connection-pool 
  "Create a connection pool. Available options are:

    :max-active 
    :max-total
    :max-idle
    :when-exhausted-action
    :test-on-borrow
    :test-on-return
    :time-between-eviction-runs-ms
    :min-evictable-idle-time-ms
    :test-while-idle
    :min-idle
    :lifo

  see
  http://commons.apache.org/pool/apidocs/org/apache/commons/pool/impl/GenericKeyedObjectPool.html
  for option documentation
  "
  [& options]
  (ConnectionPool. (make-generic-keyed-object-pool (apply hash-map options))))

