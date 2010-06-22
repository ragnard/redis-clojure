(ns redis.channel
  (:refer-clojure :exclude [send read read-line])
  (:use [redis protocol connection])
  (:import [java.io ByteArrayOutputStream]))

;;; Protocols
(defprotocol RedisChannel
  "A RedisChannel supports sending commands"
  (send [channel #^redis.protocol.RedisCommand command]))

;;; Direct channel
(defrecord DirectChannel [#^redis.connection.RedisConnection connection]
  RedisChannel
  (send [this command]
    (let [buf (ByteArrayOutputStream.)
          out (output-stream connection)
          in (input-stream connection)]
      (write-to-buffer command buf)
      (write-to-stream buf out)
      (read-reply in))))

(defn make-direct-channel [connection]
  (DirectChannel. connection))

;;; Debug channel
(defrecord DebugChannel []
  RedisChannel
  (send [this command]
        (let [buf (ByteArrayOutputStream.)]
          (write-to-buffer command buf)
          (println "Sending command")
          (println (meta command))
          (println (String. (.toByteArray buf) "ASCII"))
          nil)))

(defn make-debug-channel []
  (DebugChannel.))
