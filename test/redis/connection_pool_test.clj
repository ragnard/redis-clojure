(ns redis.connection-pool-test
  (:use [clojure.test])
  (:use [redis.connection :only (get-connection release-connection)]
        [redis.connection-pool :only (make-connection-pool)])
  (:import [java.net ServerSocket]))

(defn make-listener [port]
  (agent (ServerSocket. port)))

(defn stop [listener]
  (.close listener)
  listener)

(defn listen [listener]
  (.accept listener)
  (send-off *agent* stop)
  listener)


(defn start-listener [port]
  (send-off (make-listener port) listen))

(def node1 {:port 63110})
(def node2 {:port 63111})
(def node3 {:port 63112})

;; (deftest connection-pool
;;   (let [[l1 l2 l3] (dorun (map start-listener (map :port  [node1 node2 node3])))
;;         pool (make-connection-pool)]
;;     (Thread/sleep 1000)
;;     (let [c1 (get-connection pool node1)
;;           c2 (get-connection pool node1)]
;;       (is (not= c1 c2)))
    
;;     (shutdown-agents)
;;     )
;;   )

