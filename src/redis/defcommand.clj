(ns redis.defcommand
  (:use [clojure.string :only (upper-case)])
  (:use [redis.core :only (*channel*)]
        [redis.protocol :only (make-inline-command make-multi-bulk-command)]
        [redis.channel :only (send!)]))

;;;; Command definition macros

(defn get-key-fns [args]
  "Given a params vector, return a list of functions that knows how
  to extract keys from the given args.

  This implementation detects keys in three ways:
     - The argument is named key
     - The argument is a vector named keys
     - The argument is a vector named keyvals"
  (loop [key-fns []
         [first & rest] args]
    (if (nil? first)
      key-fns
      (condp = first
          'key (recur (conj key-fns identity) rest)
          'keys (recur (conj key-fns identity) rest)
          'keyvals (recur (conj key-fns #(take-nth 2 %)) rest)
          (recur (conj key-fns nil) rest)))))

(defn extract-keys [key-fns args]
  (vec (flatten (filter #(not (nil? %))
                        (map #(when (not (nil? %1))
                                (%1 %2)) key-fns args)))))

(def *default-opts* {:type        :multi-bulk
                     :reply-fn    identity
                     :key-fn      get-key-fns
                     :redis-shard :one})

(def *command-types* {:inline make-inline-command
                      :multi-bulk make-multi-bulk-command})


(defn parse-opts+body [opts+body]
  (loop [opts *default-opts*
         args opts+body]
    (let [[v & rest] args]
      (cond
       (nil? v) [opts nil]
       (list? v) [opts v]
       (or (var? v)
           (symbol? v)
           (fn? v)) (recur (assoc opts :reply-fn v) rest)
       (keyword? v) (condp = v
                        :inline     (recur (assoc opts :type v) rest)
                        :multi-bulk (recur (assoc opts :type v) rest)
                        :all        (recur (assoc opts :redis-shard v) rest)
                        :one        (recur (assoc opts :redis-shard v) rest))))))

(defn flatten-args [args]
  (let [[args rest] (split-with #(not= % '&) args)]
    [args (last rest)]))

(defmacro defcommand
  ([name args & opts+body]
     (let [[opts body] (parse-opts+body opts+body)
           {:keys [type reply-fn key-fn redis-shard]} opts
           command-name (upper-case name)
           command-fn (type *command-types*)
           [command-args command-rest-args] (flatten-args args)
           args-without-and (vec (filter #(not= '& %) args))
           key-fns (key-fn args-without-and)]
       (if body
         `(defn ~name ~args
            (let [command# ~body]
              (send! *channel* command#)))
         `(defn ~name ~args
            (let [command# (apply ~command-fn
                                  ~command-name
                                  ~@command-args
                                  ~command-rest-args)
                  keys# (extract-keys ~key-fns ~args-without-and)]
              (~reply-fn (send! *channel*
                                (with-meta command#
                                  {:redis-keys (vec keys#)
                                   :redis-shard ~redis-shard })))))))))

(defmacro defcommands [& command-defs]
  `(do ~@(map (fn [command-def]
                `(defcommand ~@command-def)) command-defs)))
