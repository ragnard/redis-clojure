(ns redis.pipeline
  (:refer-clojure :exclude [send read read-line])
  (:use [redis channel connection protocol])
  (:import [java.io ByteArrayOutputStream]
           [java.net SocketTimeoutException]))


(defrecord PipelinedChannel [channel commands]
  RedisChannel
  (send [this command]
    (swap! commands conj command)
    nil))

(defn make-pipelined-channel [channel]
  (PipelinedChannel. channel (atom [])))

(defn send-pipelined-commands [pipeline]
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
          (catch SocketTimeoutException e
            (throw e))
          (catch Exception e
            (conj! reply e))))
      (persistent! reply))))


