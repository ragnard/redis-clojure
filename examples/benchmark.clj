(ns benchmark
  (:require redis))

;;(when-not *command-line-args*
;;  (System/exit 0))

(def key-count 10000)

(redis/with-server
  {:host "127.0.0.1" :port 6379 :db 15}
  (do
    (redis/flushdb)
    (println "Setting keys")
    (time
     (dotimes [i key-count]
       (redis/set (str "key-" i) "value")
       ))
    (time
     (dotimes [i key-count]
       (redis/get (str "key-" i))))
    (time
     (dotimes [i key-count]
       (redis/del (str "key-" i))))))
