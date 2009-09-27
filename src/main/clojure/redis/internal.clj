(ns redis.internal
  (:refer-clojure :exclude [send read read-line])
  (:require redis.client.default)
  ;(:require redis.client.netty)
  ;(:require redis.client.nio)
  (:import [java.io Reader BufferedReader]))


(defstruct connection :host :port :db :timeout :client)

(def *connection* (struct-map connection
                    :host     "127.0.0.1"
                    :port     6379
                    :db       0
                    :timeout  5000
                    :client   nil))

(def *cr* (char 0x0d))
(def *lf* (char 0x0a))
(defn- cr? [c] (= c *cr*))
(defn- lf? [c] (= c *lf*))

(defn- uppercase [#^String s] (.toUpperCase s))
(defn- trim [#^String s] (.trim s))
(defn- parse-int [#^String s] (Integer/parseInt s))
(defn- char-array [len] (make-array Character/TYPE len))

(defn with-server*
  [server-spec func]
  (let [server (merge *connection* server-spec)
        client (redis.client/make-client server)]
    (binding [*connection* (assoc server :client client)]
      (func))))

(defn crlf?
  "Read a CR+LF combination from Reader"
  [seq]
  (let [cr (first seq)
        lf (second seq)]
    (when-not
        (and (cr? cr)
             (lf? lf))
      (throw (Exception. "Error reading CR/LF")))
    nil))


(defn read
  (let [client (*connection* client)]
    (redis.client/read client)))


;(defn read-line
;  [client]
;  (redis.client/read-line client))


(defn take-line
  [char-seq]
  (let [line (take-while (fn [e] (not= *cr* e)) char-seq)
        rest (drop (count line) char-seq)]
    (do
      (crlf? rest)
      (apply str line))))

(defn drop-crlf
  [char-seq]
  (let [cr (first char-seq)
        lf (second char-seq)]
    (if (and (cr? cr)
             (lf? lf))
      (drop 2 char-seq)
      (throw (Exception. "Did not see CRLF")))))

(defn drop-line
  [char-seq]
  (let [line (drop-while (fn [e] (not= *cr* e)) char-seq)
        rest (drop-crlf line)]
    rest))

;;
;; Reply dispatching
;;
(defn reply-type
  [client]
  (redis.client/read))

(defmulti parse-reply reply-type :default :unknown)

(defn read-reply
  ([]
     (let [reply (redis.client/reply-seq (:client *connection*))]
       (parse-reply reply)))
  ([reply]
     (parse-reply (seq reply))))

(defmethod parse-reply :unknown
  [reply]
  (throw (Exception. (str "Unknown reply type: " (first reply)))))

(defmethod parse-reply \-
  [reply]
  (let [error (take-line (rest reply))]
    (throw (Exception. (str "Server error: " error)))))

(defmethod parse-reply \+
  [reply]
  (take-line (rest reply)))

(defmethod parse-reply \$
  [reply]
  (let [message (rest reply)
        line (take-line message)
        length (parse-int line)]
    (if (< length 0)
      nil
      (let [rest (drop (+ (count line) 2) (rest reply))
            data (take length rest)
            end (drop length rest)]
        (cond
         (not= length (count data)) (throw (Exception. "Could not read correct number of bytes"))
         (crlf? end) (throw (Exception. "Could not read terminating CR/LF"))
         true (apply str data))))))
                       
(defmethod parse-reply \*
  [reply]
  (let [data (rest reply)
        line (take-line data)
        count (parse-int line)]
    (prn data)
    (prn line)
    (prn count)
    (if (< count 0)
      nil
      (loop [i count
             replies []]
        (prn replies)
        (if (zero? i)
          replies
          (recur (dec i) (conj replies (read-reply (drop-line data)))))))))
  

(defmethod parse-reply \:
  [reply]
  (let [line (trim (take-line (rest reply)))
        int (parse-int line)]
    int))



;;
;; Command functions
;;
(defn- str-join
  "Join elements in sequence with separator"
  [separator sequence]
  (apply str (interpose separator sequence)))


(defn inline-command
  "Create a string for an inline command"
  [name & args]
  (let [cmd (str-join " " (conj args name))]
    (str cmd "\r\n")))

(defn bulk-command
  "Create a string for a bulk command"
  [name & args]
  (let [data (str (last args))
        data-length (count (str data))
        args* (concat (butlast args) [data-length])
        cmd (apply inline-command name args*)]
    (str cmd data "\r\n")))


(defn- sort-command-args-to-string
  [args]
  (loop [arg-strings []
         args args]
    (if (empty? args)
      (str-join " " arg-strings)
      (let [type (first args)
            args (rest args)]
        (condp = type
          :by (let [pattern (first args)]
                (recur (conj arg-strings "BY" pattern)
                       (rest args)))
          :limit (let [start (first args)
                       end (second args)]
                   (recur (conj arg-strings "LIMIT" start end)
                          (drop 2 args)))
          :get (let [pattern (first args)]
                 (recur (conj arg-strings "GET" pattern)
                        (rest args)))
          :alpha (recur (conj arg-strings "ALPHA") args)
          :asc  (recur (conj arg-strings "ASC") args)
          :desc (recur (conj arg-strings "DESC") args)
          (throw (Exception. (str "Error parsing SORT arguments: Unknown argument: " type))))))))

(defn sort-command
  [name & args]
  (when-not (= name "SORT")
    (throw (Exception. "Sort command name must be 'SORT'")))
  (let [key (first args)
        arg-string (sort-command-args-to-string (rest args))
        cmd (str "SORT " key)]
    (if (empty? arg-string)
      (str cmd "\r\n")
      (str cmd " " arg-string "\r\n"))))


(def command-fns {:inline 'inline-command
                  :bulk   'bulk-command
                  :sort   'sort-command})


(defn parse-params
  "Return a restructuring of params, which is of form:
     [arg* (& more)?]
  into
     [(arg1 arg2 ..) more]"
  [params]
  (let [[args rest] (split-with #(not= % '&) params)]
    [args (last rest)]))

(defmacro defcommand
  "Define a function for Redis command name with parameters
  params. Type is one of :inline, :bulk or :sort, which determines how
  the command string is constructued."
  ([name params type] `(defcommand ~name ~params ~type (fn [reply#] reply#)))
  ([name params type reply-fn] `(~name ~params ~type ~reply-fn)
     (do
       (let [command (uppercase (str name))
             command-fn (type command-fns)
             [command-params
              command-params-rest] (parse-params params)]
         `(defn ~name
            ~params
            (let [request# (apply ~command-fn
                                  ~command
                                  ~@command-params
                                  ~command-params-rest)]
              ;(send-command request#)
              (redis.client/send (:client *connection*) request#)
              (~reply-fn (read-reply)))))
       
       )))


(defmacro defcommands
  [& command-defs]
  `(do ~@(map (fn [command-def]
              `(defcommand ~@command-def)) command-defs)))



