(ns redis.client.default
  (:refer-clojure :exclude [send])
  (:use redis.client)
  (:import [java.net Socket])
  (:import [java.io Reader BufferedReader InputStreamReader]))

(defn- reader-seq
  [#^Reader reader]
  (let [c (.read reader)]
    (if (< 0 c)
      (lazy-seq
       (concat [(char c)] (reader-seq reader))))))

(defmethod make-client :default [server-spec]
  (let [{:keys [host port timeout]} server-spec
        socket (Socket. #^String host #^Integer port)]
    (doto socket
      (.setSoTimeout timeout)
      (.setTcpNoDelay true)
      (.setKeepAlive true))))


(defmethod send :default
  [#^Socket socket #^String command]
  (let [out (.getOutputStream socket)
        bytes (.getBytes command)]
    (.write out bytes)))

(defmethod get-reader :default
  [#^Socket socket]
  (let [in (.getInputStream socket)
        reader (BufferedReader. (InputStreamReader. in))]
    reader))

(defmethod reply-seq :default
  [#^Socket socket]
  (reader-seq (InputStreamReader. (.getInputStream socket))))

