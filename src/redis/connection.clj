(ns redis.connection
  (:use [redis.protocol :only (write-to-buffer write-to-stream)])
  (:import [java.net Socket]
           [java.io BufferedInputStream ByteArrayOutputStream]))

;;; Protocols

(defprotocol RedisConnectionPool
  (get-connection [pool connection-spec])
  (release-connection [pool connection]
                      [pool connection exception]))

(defprotocol RedisConnection
  (get-server-spec [connection])
  (close [connection])
  (input-stream [connection])
  (output-stream [connection]))


;;; Macros

(defmacro with-connection [name pool server-spec & body]
  `(let [~name (get-connection ~pool ~server-spec)]
     (try
       ~@body
       (catch Exception e#
         (release-connection ~pool ~name e#)
         (throw e#))
       (finally
        (release-connection ~pool ~name)))))


;;; Implementations
(defrecord Connection [#^Socket socket server-spec]
  RedisConnection
  (get-server-spec [this] server-spec)
  (close [this] (.close socket))
  (input-stream [this] (BufferedInputStream. (.getInputStream socket)))
  (output-stream [this] (.getOutputStream socket)))

(defn send-command-and-read-reply
  [connection command]
  (let [buf (ByteArrayOutputStream.)
        in (input-stream connection)
        out (output-stream connection)]
    (write-to-buffer command buf)
    (write-to-stream buf out)
    (redis.protocol/read-reply in)))

(def default-connection-spec {:host "127.0.0.1"
                              :port 6379
                              :timeout 5000
                              :password nil
                              :db 0})


(defn make-connection [server-spec]
  (let [spec (merge default-connection-spec server-spec)
        {:keys [host port timeout]} spec
        socket (Socket. #^String host #^Integer port)]
    (doto socket
      (.setTcpNoDelay true)
      (.setSoTimeout timeout))
    (Connection. socket server-spec)))

(defrecord NonPooledConnectionPool []
  RedisConnectionPool
  (get-connection [this connection-spec]
    (make-connection connection-spec))
  (release-connection [this connection]
    (close connection))
  (release-connection [this connection exception]
    (close connection)))

(defn make-non-pooled-connection-pool []
  (NonPooledConnectionPool.))



