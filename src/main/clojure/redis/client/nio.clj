(ns redis.client.nio
  (:refer-clojure :exclude [send read read-line])
  (:use redis.client)
  (:import [java.net InetSocketAddress Socket]
           [java.nio ByteBuffer]
           [java.nio.channels SocketChannel]))

(defstruct nio-client :channel :read-buffer)


(defmethod make-client :nio 
  [server-spec]
  (let [{:keys [host port timeout]} server-spec
        channel (SocketChannel/open (InetSocketAddress. #^String host #^Integer port))
        socket (.socket channel)]
    (doto socket
      (.setSoTimeout timeout)
      (.setTcpNoDelay true))
    (doto channel
      (.configureBlocking true))
    (struct nio-client channel (ByteBuffer/allocateDirect 4096))))

(defmethod send :nio
  [nio-client #^String command]
  (let [buffer (:buffer nio-client)]
    ()))




;
; 
;
;
