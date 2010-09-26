(ns redis.channel
  (:use [redis.protocol :only (read-reply write-to-buffer write-to-stream)]
        [redis.connection :only (RedisConnection input-stream output-stream)])
  (:import [java.io ByteArrayOutputStream]))

;;; Protocols
(defprotocol RedisChannel
  "A RedisChannel supports sending commands"
  (send! [channel #^redis.protocol.RedisCommand command]))

;;; Direct channel
(defrecord DirectChannel [#^redis.connection.RedisConnection connection]
  RedisChannel
  (send! [this command]
    (let [buf (ByteArrayOutputStream.)
          out (output-stream connection)
          in (input-stream connection)]
      (write-to-buffer command buf)
      (write-to-stream buf out)
      (read-reply in))))

(defn make-direct-channel [connection]
  (DirectChannel. connection))


