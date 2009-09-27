(ns redis.client
  (:refer-clojure :exclude [send read read-line]))

(def *client-type* :default)
(defn- client-type [] *client-type*)

(defn with-client*
  [type func]
  (binding [*client-type* type]
    (func)))

(defmacro with-client
  [type body]
  `(with-client* ~type (fn [] ~@body)))

(defmulti make-client client-type)

(defmulti read client-type)
(defmulti read-line client-type)

(defmulti send client-type)


