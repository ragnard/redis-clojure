(ns redis.protocol
  (:refer-clojure :exclude [read read-line send])
  (:import [java.net Socket]
           [java.io InputStream
                    OutputStream
                    BufferedInputStream
                    BufferedOutputStream
                    ByteArrayOutputStream
                    ByteArrayInputStream]))

;;;
;;; Protocols
;;;
(defprotocol RedisInputStream
  (#^String read [stream count])
  (read-crlf [stream])
  (#^String read-line [stream]))

;;;
;;; Reply parsing
;;;

(def #^String *string-charset* "UTF-8")

(def CR (byte 0x0d))
(def LF (byte 0x0a))
(def CRLF (byte-array 2 [CR LF]))
(defn- CR? [b] (= b CR))
(defn- LF? [b] (= b LF))
(defn- to-ascii [value] (.getBytes (str value) "ASCII"))


(defn- read-expected-byte [#^InputStream stream expected]
  (let [actual (.read stream)
        expected (int expected)]
    (cond
     (= actual expected) actual
     (= actual -1) (throw (Exception. "End of stream reached"))
     true (throw (Exception. (format "Expected byte: 0x%1$x, read 0x%2$x" expected actual))))))


(extend-type BufferedInputStream
  RedisInputStream
  (read [this count]
    (let [buf (byte-array count)
          nread (.read this buf 0 count)]
      (when (= -1 nread)
        (throw (Exception. (str "End of stream reached"))))
      (when (not= nread count)
        (throw (Exception. (str "Unable to read" count "bytes, read" nread "bytes"))))
      (String. buf *string-charset*)))

  (read-crlf [this]
    (read-expected-byte this CR)
    (read-expected-byte this LF))

  (read-line [this]
    (let [buf (ByteArrayOutputStream.)]
      (loop [byte (.read this)]
        (when (= byte -1)
          (throw (Exception. "End of stream reached")))
        (if (CR? byte)
          (let [next (.read this)]
            (if (LF? next)
              (String. (.toByteArray buf) *string-charset*)
              (throw (Exception. "Read CR not followed by LF"))))
          (do
            (.write buf byte)
            (recur (.read this))))))))


;;
;; Replies
;;
(declare read-reply)

(defn- parse-int [string] (Integer/parseInt string))

(defn- read-error-reply [channel]
  (let [message (read-line channel)]
    (throw (Exception. message))))

(defn- read-single-line-reply [channel]
  (read-line channel))

(defn- read-bulk-reply [channel]
  (let [line (read-line channel)
        len (parse-int line)]
    (if (neg? len)
      nil
      (let [bulk (read channel len)]
        (read-crlf channel)
        bulk))))

(defn- read-multi-bulk-reply [channel]
  (let [line (read-line channel)
        nbulks (parse-int line)]
    (if (< nbulks 0)
      nil
      (loop [i nbulks
             bulks []]
        (if (zero? i)
          bulks
          (recur (dec i) (conj bulks (read-reply channel))))))))

(defn- read-integer-reply [channel]
  (let [number (read-line channel)]
    (parse-int number)))

(defn read-reply [channel]
  (let [type (read channel 1)]
    (case type
      "-" (read-error-reply channel)
      "+" (read-single-line-reply channel)
      "$" (read-bulk-reply channel)
      "*" (read-multi-bulk-reply channel)
      ":" (read-integer-reply channel)
      (throw (Exception. (format "Unknown reply type: %1$c" type))))))

;;
;; Commands
;;
(defprotocol RedisBuffer
  "A RedisBuffer supports putting bytes in, and writing them out to a
Stream"
  (put-byte [buf byte])
  (put-bytes [buf #^bytes bytes])
  (write-to-stream [buf #^Stream stream]))

(defprotocol RedisCommand
  "A RedisCommand knows how to write itself to a RedisBuffer"
  (write-to-buffer [command buf]))

;; Extend ByteArrayOutputStream to support the RedisBuffer protocol
(extend-type ByteArrayOutputStream
  RedisBuffer
  (put-byte [this #^Integer byte] (.write this byte))
  (put-bytes [this #^bytes bytes] (.write this bytes 0 (alength bytes)))
  (write-to-stream [this stream] (.writeTo this stream)))

;; Inline command
(defrecord InlineCommand [#^String name]
  RedisCommand
  (write-to-buffer [this buf]
    (let [bytes (to-ascii name)]
      (put-bytes buf bytes)
      (put-bytes buf CRLF)))

  Object
  (toString [this]
    (let [buf (ByteArrayOutputStream.)]
      (write-to-buffer this buf)
      (str buf))))

(defn make-inline-command [name]
  (InlineCommand. name))

;; Multi bulk command
(def bulk-count-marker (int \*))
(def bulk-length-marker (int \$))

(defn- write-bulk-header [buf nbulks]
  (put-byte buf bulk-count-marker)
  (put-bytes buf (to-ascii nbulks))
  (put-bytes buf CRLF))

(defn- write-bulk [buf bulk]
  (let [s (str bulk)
        data (.getBytes s *string-charset*)
        len (alength data)]
    (put-byte buf bulk-length-marker)
    (put-bytes buf (to-ascii len))
    (put-bytes buf CRLF)
    (put-bytes buf data)
    (put-bytes buf CRLF)))

(defrecord MultiBulkCommand [bulks]
  RedisCommand
  (write-to-buffer [this buf]
    (let [nbulks (count bulks)]
      (write-bulk-header buf nbulks)
      (dorun
       (map #(write-bulk buf %) bulks))))

  Object
  (toString [this]
    (let [buf (ByteArrayOutputStream.)]
      (write-to-buffer this buf)
      (str buf))))

(defn make-multi-bulk-command [& args]
  (when (empty? args)
    (throw (IllegalArgumentException.
            "At least one argument is required for multi bulk commands")))
  (MultiBulkCommand. args))

