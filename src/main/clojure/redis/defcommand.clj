(ns redis.defcommand
  (:require [redis protocol channel]))

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

(def *default-opts* {:type     :multi-bulk
                     :reply-fn identity
                     :key-fn   get-key-fns})

(def *command-types* {:inline redis.protocol/make-inline-command
                      :multi-bulk redis.protocol/make-multi-bulk-command})


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
                        :multi-bulk (recur (assoc opts :type v) rest))))))

(defn flatten-args [args]
  (let [[args rest] (split-with #(not= % '&) args)]
    [args (last rest)]))

(defn- upcase [#^String s] (.toUpperCase s))

(defmacro defcommand
  ([name args & opts+body]
     (let [[opts body] (parse-opts+body opts+body)
           {:keys [type reply-fn key-fn]} opts
           command-name (upcase (str name))
           command-fn (type *command-types*)
           [command-args command-rest-args] (flatten-args args)
           args-without-and (vec (filter #(not= '& %) args))
           key-fns (key-fn args-without-and)]
       (if body
         `(defn ~name ~args
            (let [command# ~body]
              (redis.channel/send redis/*channel* command#)))
         `(defn ~name ~args
            (let [command# (apply ~command-fn
                                  ~command-name
                                  ~@command-args
                                  ~command-rest-args)
                  keys# (extract-keys ~key-fns ~args-without-and)]
              (~reply-fn (redis.channel/send redis/*channel*
                                     (with-meta command#
                                       {:redis-keys (vec keys#)})))))))))

(defmacro defcommands [& command-defs]
  `(do ~@(map (fn [command-def]
                `(defcommand ~@command-def)) command-defs)))
