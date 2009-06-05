(ns redis.internal
  (:import [java.io InputStream 
                    OutputStream
                    Reader
                    InputStreamReader
                    BufferedReader]
           [java.net Socket]))

(set! *warn-on-reflection* true)

(def *cr* 0x0d)
(def *lf* 0x0a)
(defn- cr? [c] (= c *cr*))
(defn- lf? [c] (= c *lf*))

(defn- uppercase [#^String s] (.toUpperCase s))
(defn- trim [#^String s] (.trim s))
(defn- parse-int [#^String s] (Integer/parseInt s))
(defn- char-array [len] (make-array Character/TYPE len))

(def *default-host* "127.0.0.1")
(def *default-port* 6379)
(def *default-db* 0)
(def *default-timeout* 5) ;; not currently used


(defstruct server :host :port :db :timeout :socket)

(def *server* (struct-map server
                :host     *default-host*
                :port     *default-port*
                :db       *default-db*
                :timeout  *default-timeout*
                :socket   nil))

(defn connect-to-server
  "Create a Socket connected to server"
  [server]
  (let [{:keys [host port timeout]} server
        socket (Socket. #^String host #^Integer port)]
    socket))

(defn with-server*
  [server-spec func]
  (let [server (merge *server* server-spec)]
    (with-open [#^Socket socket (connect-to-server server)]
      (binding [*server* (assoc server :socket socket)]
        (func)))))

(defn socket* []
  (or (:socket *server*)
      (throw (Exception. "Not connected to a Redis server"))))

(defn send-command
  "Send a command string to server"
  [#^String cmd]
  (let [out (.getOutputStream (#^Socket socket*))
        bytes (.getBytes cmd)]
    (.write out bytes)))



(defn read-crlf
  [#^Reader reader]
  (let [cr (.read reader)
        lf (.read reader)]
    (when-not
        (and (cr? cr)
             (lf? lf))
      (throw (Exception. "Error reading CR/LF")))
    nil))

(defn read-line-crlf
  [#^Reader reader]
  (loop [line []
         c (.read reader)]
    (when (< c 0)
      (throw (Exception. "Error reading line: EOF reached")))
    (if (cr? c)
      (let [next (.read reader)]
        (if (lf? next)
          (apply str line)
          (throw (Exception. "Error reading line: Missing LF"))))
      (recur (conj line (char c))
             (.read reader)))))

(defn read-error-reply
  [#^BufferedReader reader]
  (let [error (read-line-crlf reader)]
    (throw (Exception. (str "Server error: " error)))))

(defn read-integer-reply
  [#^BufferedReader reader]
  (let [number (trim (read-line-crlf reader))]
    (parse-int number)))

(defn read-bulk-reply
  [#^BufferedReader reader]
  (let [line (read-line-crlf reader)
        length (parse-int line)]
    (if (< length 0)
      nil
      (let [#^chars cbuf (char-array length)
            nread (.read reader cbuf 0 length)]
        (if (not= nread length)
          (throw (Exception. "Could not read correct number of bytes"))
          (do
            (read-crlf reader) ;; CRLF
            (String. cbuf)))))))

(def read-reply) ;; forward declaration

(defn read-multi-bulk-reply
  [#^BufferedReader reader]
  (let [line (read-line-crlf reader)
        count (parse-int line)]
    (if (< count 0)
      nil
      (loop [i count
             replies []]
        (if (zero? i)
          replies
          (recur (dec i) (conj replies (read-reply reader))))))))

(defn read-reply
  ([] 
     (let [input-stream (.getInputStream (#^Socket socket*))
           reader (BufferedReader. (InputStreamReader. input-stream))]
       (read-reply reader)))
  ([#^BufferedReader reader]
     (let [type (char (.read reader))]
       (condp = type
         \- (read-error-reply reader)
         \+ (read-line-crlf reader)
         \$ (read-bulk-reply reader)
         \* (read-multi-bulk-reply reader)
         \: (read-integer-reply reader)
         (throw (Exception. (str "Protocol error: Unknown reply type: " type)))))))




(defn str-join
  [separator sequence]
  (apply str (interpose separator sequence)))


(defn command-str
  [coll]
  (str (str-join " " coll) "\r\n"))

(defn inline-command
  [name & args]
  (let [cmd (str-join " " (conj args name))]
    (str cmd "\r\n")))

(defn bulk-command
  [name & args]
  (let [data (str (last args))
        data-length (count (str data))
        args* (concat (butlast args) [data-length])
        cmd (apply inline-command name args*)]
    (str cmd data "\r\n")))


(def command-fns {:inline 'inline-command
                  :bulk   'bulk-command})


(defn parse-params
  [params]
  (let [[args rest] (split-with #(not= % '&) params)]
    [args (last rest)]))

(defmacro defcommand
  ([name params type] `(defcommand ~name ~params ~type (fn [reply#] reply#)))
  ([name params type reply-fn] `(~name ~params ~type ~reply-fn)
     (do
       (when (name (ns-map *ns*))
         (let [var (ns-resolve *ns* name)
               namespace (ns-name *ns*)]
           ;(println (str "Warning: Changing mapping of '" name "' in '" namespace "'\n  Previous mapping: " var  "\n  New mapping: #'" namespace "/" name))
           (ns-unmap *ns* name)))
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
              (send-command request#)
              (~reply-fn (read-reply)))))
       
       )))


(defmacro defcommands
  [& command-defs]
  `(do ~@(map (fn [command-def]
              `(defcommand ~@command-def)) command-defs)))



