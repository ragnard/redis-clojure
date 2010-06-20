(ns redis.connection
  (:refer-clojure :exclude [read-line send])
  (:import [java.net Socket]
           [java.io BufferedInputStream]))

;;; Protocols
(defprotocol RedisConnection
  (close [connection])
  (input-stream [connection])
  (output-stream [connection]))

(defprotocol RedisConnectionPool
  (get-connection [pool connection-spec])
  (release-connection [pool connection]))


(extend-type Socket
  RedisConnection
  (close [this] (.close this))
  (input-stream [this] (BufferedInputStream. (.getInputStream this)))
  (output-stream [this] (.getOutputStream this)))


(def default-connection-spec {:host "127.0.0.1"
                              :port 6379
                              :timeout 5000
                              :password nil
                              :db 0})


(defn make-connection [connection-spec]
  (let [spec (merge default-connection-spec connection-spec)
        {:keys [host port timeout]} spec
        socket (Socket. #^String host #^Integer port)]
    (doto socket
      (.setTcpNoDelay true)
      (.setSoTimeout timeout))))

(defrecord NonPooledConnectionPool []
  RedisConnectionPool
  (get-connection [this connection-spec]
    (make-connection connection-spec))
  (release-connection [this connection]
     (close connection)))

(defn make-non-pooled-connection-pool []
  (NonPooledConnectionPool.))







