(ns redis.client.default
  (:refer-clojure :exclude [send])
  (:use redis.client)
  (:import [java.net Socket]))


(defmethod connect :default [host port timeout]
  (let [socket (Socket. #^String host #^Integer port)]
    (doto socket
      (.setSoTimeout timeout)
      (.setTcpNoDelay true)
      (.setKeepAlive true))))


(defmethod send :default [#^Socket socket
                          #^String command]
  (let [out (.getOutputStream socket)
        bytes (.getBytes command)]
    (.write out bytes)))

(defmethod get-reader :default [#^Socket socket]
  (let [in (.getInputStream socket)
        redaer (BufferedReader. (InputStreamReader in))]
    reader))

                                        ;(defmethod get-re :default [socket])