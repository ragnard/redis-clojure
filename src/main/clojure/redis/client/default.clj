(ns redis.client.default
  (:refer-clojure :exclude [send read read-line])
  (:use redis.client)
  (:import [java.net Socket])
  (:import [java.io Reader BufferedReader InputStreamReader]))


(defstruct client-data :socket :reader)

(defn- buffered-reader-for-socket
  [#^Socket socket]
  (let [stream (.getInputStream socket)
        reader (BufferedReader. (InputStreamReader. stream))]
    reader))

(defmethod make-client :default [server-spec]
  (let [{:keys [host port timeout]} server-spec
        socket (Socket. #^String host #^Integer port)
        reader (buffered-reader-for-socket socket)]
    (doto socket
      (.setSoTimeout timeout)
      (.setTcpNoDelay true)
      (.setKeepAlive true))
    (struct client-data socket reader)))

(defmethod send :default
  [client-data #^String command]
  (let [socket (client-data :socket)
        out (.getOutputStream socket)
        bytes (.getBytes command)]
    (.write out bytes)))

(defmethod read :default
  [client-data count]
  (let [reader (client-data :reader)]
    (char (.read reader))))

(defmethod read-line :default
  [client-data]
  (let [reader (client-data :reader)]
    (.readLine reader)))


