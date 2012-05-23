(ns redis
  (:refer-clojure :exclude [get set type keys sort])
  (:use redis.internal))

(defmacro with-server
  "Evaluates body in the context of a new connection to a Redis server
  then closes the connection.

  server-spec is a map with any of the following keys:
    :host     hostname (default \"127.0.0.1\")
    :port     port (default 6379)
    :db       database to use (default 0)"
  [server-spec & body]
  `(with-server* ~server-spec (fn []
                                (do
                                  (if (:password *connection*)
                                    (redis/auth (:password *connection*)))
                                  (redis/select (:db *connection*))
                                  ~@body))))

(defmacro atomically
  "Execute all redis commands in body atomically, ie. sandwiched in a
  MULTI/EXEC statement. If an exception is thrown the EXEC command
  will be terminated by a DISCARD, no operations will be performed and
  the exception will be rethrown." 
  [& body]
  `(do 
     (redis/multi) 
     (try
      (do 
        ~@body
        (redis/exec))
      (catch Exception e#
        (redis/discard)
        (throw e#)))))

;;
;; Reply conversion functions
;;
(defn int-to-bool
  "Convert integer reply to a boolean value"
  [int]
  (= 1 int))

(defn string-to-keyword
  "Convert a string reply to a keyword"
  [string]
  (keyword string))

(defn string-to-map
  "Convert strings with format 'key:value\r\n'+ to a map with {key
  value} pairs"
  [#^String string]
  (let [lines (.split string "(\\r\\n|:)")]
    (apply hash-map lines)))

(defn string-to-double
  "Convert a string in floating point format to a double"
  [string]
  (if string
    (Double/parseDouble string)))

(defn int-to-date
  "Return a Date representation of a UNIX timestamp"
  [int]
  (new java.util.Date (long int)))

(defn seq-to-set
  [sequence]
  (clojure.core/set sequence))

;;
;; Commands
;;
(defcommands
  ;; Connection handling
  (auth        [password] :inline)
  (quit        [] :inline)
  (ping        [] :inline)
  ;; String commands
  (set         [key value] :bulk)
  (get         [key] :inline)
  (getset      [key value] :bulk)
  (setnx       [key value] :bulk int-to-bool)
  (incr        [key] :inline)
  (incrby      [key integer] :inline)
  (decr        [key] :inline)
  (decrby      [key integer] :inline)
  (exists      [key] :inline int-to-bool)
  (mget        [key & keys] :inline)
  (mset        [key value & more] :bulk)
  (msetnx      [key value & more] :bulk int-to-bool)
  (del         [key] :inline int-to-bool)
  ;; Key space commands
  (type        [key] :inline string-to-keyword)
  (keys        [pattern] :inline)
  (randomkey   [] :inline)
  (rename      [oldkey newkey] :inline)
  (renamenx    [oldkey newkey] :inline int-to-bool)
  (dbsize      [] :inline)
  (expire      [key seconds] :inline int-to-bool)
  (ttl         [key] :inline)
  ;; List commands
  (rpush       [key value] :bulk)
  (lpush       [key value] :bulk)
  (llen        [key] :inline)
  (lrange      [key start end] :inline)
  (ltrim       [key start end] :inline)
  (lindex      [key index] :inline)
  (lset        [key index value] :bulk)
  (lrem        [key count value] :bulk)
  (lpop        [key] :inline)
  (rpop        [key] :inline)
  (rpoplpush   [srckey dstkey] :inline)
  ;; Set commands
  (sadd        [key member] :bulk int-to-bool)
  (srem        [key member] :bulk int-to-bool)
  (spop        [key] :inline)
  (smove       [srckey destkey member] :bulk int-to-bool)
  (scard       [key] :inline)
  (sismember   [key member] :bulk int-to-bool)
  (sinter      [key & keys] :inline seq-to-set)
  (sinterstore [destkey key & keys] :inline)
  (sunion      [key & keys] :inline seq-to-set)
  (sunionstore [destkey key & keys] :inline)
  (sdiff       [key & keys] :inline seq-to-set)
  (sdiffstore  [destkey key & keys] :inline)
  (smembers    [key] :inline seq-to-set)
  ;; ZSet commands
  (zadd        [key score member] :bulk int-to-bool)
  (zrem        [key member] :bulk int-to-bool)
  (zincrby     [key increment member] :bulk string-to-double)
  (zscore      [key member] :bulk string-to-double)
  (zcard       [key] :inline)
  (zrange      [key start end] :inline)
  (zrevrange   [key start end] :inline)
  (zrangebyscore [key start end] :inline)
  (zremrangebyscore [key start end] :inline)
  (zrank [key member] :bulk)
  ;; Hash commands
  (hset        [key field value] :bulk int-to-bool)
  (hget        [key field] :bulk);;;;
  (hmset       [key field value & more] :bulk)
  (hincrby     [key field integer] :inline)
  (hexists     [key field] :bulk int-to-bool);;;
  (hdel        [key field] :bulk int-to-bool);;;
  (hlen        [key] :inline)
  (hkeys       [key] :inline)
  (hvals       [key] :inline)
  (hgetall     [key] :inline)  
  ;; MULTI/EXEC/DISCARD
  (multi       [] :inline)
  (exec        [] :inline)
  (discard     [] :inline)
  ;; Multiple database handling commands
  (select      [index] :inline)
  (move        [key dbindex] :inline)
  (flushdb     [] :inline)
  (flushall    [] :inline)
  ;; Sorting
  (sort        [key & options] :sort)
  ;; Persistence
  (save        [] :inline)
  (bgsave      [] :inline)
  (bgrewriteaof [] :inline)
  (lastsave    [] :inline int-to-date)
  (shutdown    [] :inline)
  ;; Remote control
  (info        [] :inline string-to-map)
  ;;(monitor     [] :inline))
)
