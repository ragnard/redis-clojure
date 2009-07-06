(ns redis.client
  (:refer-clojure :exclude [send]))

(def *client-type* :default)

(defmulti connect (fn [& rest] *client-type*))
(defmulti send (fn [& rest] *client-type*))
(defmulti receive (fn [& rest] *client-type*))

