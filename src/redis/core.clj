(ns redis.core
  (:refer-clojure :exclude [keys type get set sort])
  (:require [clojure.contrib.ns-utils :only (immigrate) :as contrib])
  (:use [redis.connection :only (with-connection make-non-pooled-connection-pool)]
        [redis.connection-pool :only (make-connection-pool)]
        [redis.channel :only (make-direct-channel)]))

;;;; Vars

(def #^{:doc "Bound to an implementation of RedisConnectionPool"}
     *pool*
     (make-connection-pool :lifo false
                           :test-on-borrow true))

(def #^{:doc "Bound to an implementation of RedisChannel"}
     *channel* nil)

;;;; Macros

(defmacro with-server
  "Evaluates body in the context of a connection to Redis server
  specified by server-spec.

  server-spec is a map with any of the following keys:
    :host     (\"127.0.0.1\")
    :port     (6379)
    :db       (0)
    :timeout  (5000)
    :password (nil)"
  ([server-spec & body]
     `(with-connection connection# *pool* ~server-spec
        (binding [*channel* (make-direct-channel connection#)]
          ~@body))))

;; Immigrate commands
(contrib/immigrate 'redis.commands)