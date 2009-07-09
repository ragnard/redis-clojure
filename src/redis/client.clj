(ns redis.client
  (:refer-clojure :exclude [send])
  ;(:require redis.client.default)
  )

(def *client-type* :default)

(defmulti make-client (fn [& _] *client-type*))

(defmulti response-seq (fn [& _] *client-type*))


(defmulti send (fn [& _] *client-type*))
(defmulti get-reader (fn [& _] *client-type*))

