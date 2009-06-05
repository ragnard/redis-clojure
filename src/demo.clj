(ns demo
  (:require redis))


(redis/with-server
  {:host "127.0.0.1" :port 6379 :db 0}
  (do
    (println (redis/ping))
    ;(println (redis/info))
    (let [info (redis/info)]
      (println info)
      (apply map (fn [entry] (println entry)) info))))

