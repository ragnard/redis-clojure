(add-classpath "file:///Users/ragge/Projects/clojure/redis-clojure/src/")

(ns redis
  (:use clojure.contrib.pprint)
  (:use redis.internal))


(defmacro with-server
  "Evaluates body in the context of a new connection to a Redis server
  then closes the connection."
  [server-spec & body]
  `(with-server* ~server-spec (fn []
                                (do
                                  (redis/select (:db *server*))
                                  ~@body))))


;;
;; Reply conversion functions
;;
(defn int-to-bool
  "Convert integer reply to a boolean value"
  [int]
  (= 1 int))

(defn string-to-type
  "Convert a string reply to a Redis type"
  [string]
  (keyword string))

(defn string-to-seq
  "Convert a space separated string to a sequence of words"
  [#^String string]
  (if (empty? string)
    nil
    (seq (.split string "\\s+"))))

(defn string-to-map
  "Convert strings with format 'key:value\r\n'+ to a map with {key value} pairs"
  [#^String string]
  (let [lines (.split string "(\\r\\n|:)")]
    (apply hash-map lines)))

;;
;; Commands
;;
(defcommands
  (quit      [] :inline)
  (ping      [] :inline)
  (set       [key value] :bulk)
  (get       [key] :inline)
  (getset    [key value] :bulk)
  (setnx     [key value] :bulk int-to-bool)
  (incr      [key] :inline)
  (incrby    [key integer] :inline)
  (decr      [key] :inline)
  (decrby    [key integer] :inline)
  (exists    [key] :inline int-to-bool)
  (mget      [key & keys] :inline)
  (del       [key] :inline int-to-bool)
  (type      [key] :inline string-to-type)
  (keys      [pattern] :inline string-to-seq)
  (randomkey [] :inline)
  (rename    [oldkey newkey] :inline)
  (renamenx  [oldkey newkey] :inline int-to-bool)
  (dbsize    [] :inline)
  (expire    [key seconds] :inline int-to-bool)
  (ttl       [key] :inline)
  (rpush     [key value] :bulk)
  (lpush     [key value] :bulk)
  (llen      [key] :inline)
  (lrange    [key start end] :inline)
  (lindex    [key index] :inline)
  (select    [index] :inline)
  (move      [key dbindex] :inline)
  (flushdb   [] :inline)
  (info      [] :inline string-to-map))


