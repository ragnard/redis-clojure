(ns redis.channels
  (:refer-clojure :exclude [send read read-line])
  (:use [redis protocol connection])
  (:import [java.io ByteArrayOutputStream]))

;;; Protocols
(defprotocol RedisChannel
  "A RedisChannel supports sending commands"
  (send [channel #^redis.protocol.RedisCommand command]))

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

;;; Direct channel
(defrecord DirectChannel [#^redis.connection.RedisConnection connection]
  RedisChannel
  (send [this command]
    (let [buf (ByteArrayOutputStream.)
          out (output-stream connection)
          in (input-stream connection)
          keys (:redis-keys (meta command))]
      (write-to-buffer command buf)
      (write-to-stream buf out)
      (read-reply in))))

(defn make-direct-channel [connection]
  (DirectChannel. connection))

;;; Pipelining
(defrecord PipelinedChannel [channel commands]
  RedisChannel
  (send [this command]
    (swap! commands conj command)
    nil))

(defn make-pipelined-channel [channel]
  (PipelinedChannel. channel (atom [])))

(defn send-pipelined-commands [pipeline]
  (do
    (let [buf (ByteArrayOutputStream.)
          channel (:channel pipeline)
          connection (:connection channel)
          commands @(:commands pipeline)
          ncommands (count commands)
          out (output-stream connection)
          in (input-stream connection)]
      (dorun
       (map #(write-to-buffer % buf) commands))
      (write-to-stream buf out)
      (let [reply (transient [])]
        (dotimes [i ncommands]
          (try 
            (conj! reply (read-reply in))
            (catch Exception e
              (conj! reply e))))
        (persistent! reply)))))


