(add-classpath "file:///Users/ragge/Projects/clojure/redis-clojure/redis-clojure.jar")

(println (System/getProperty "java.class.path"))

(ns demo
  (:require redis))



(redis/with-server
  {:host "127.0.0.1" :port 6379 :db 0}
  (do
    (println "Sending ping")
    (println "Reply:" (redis/ping))
    (println "Server info:")
    (let [info (redis/info)]
      (dorun
       (map (fn [entry]
              (println (str "- "(first entry) ": " (last entry)))) info)))
    (println "Setting key 'foo' to 'bar'")
    (println "Reply:" (redis/set "foo" "bar"))
    (println "Getting value of key 'foo'")
    (println "Reply:" (redis/get "foo"))))

