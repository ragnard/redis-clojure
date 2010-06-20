(ns redis.protocol-tests
  (:refer-clojure :exclude [read read-line])
  (:use [redis.protocol] 
        [clojure.test])
  (:import [java.io BufferedInputStream
                    ByteArrayInputStream]))


(deftest inline-command
  (testing "An inline command"
    (testing "should write itself as a CRLF terminated string"
      (let [c (make-inline-command "PING")]
        (is (= "PING\r\n"
               (str c)))))))

(deftest multi-bulk-command
  (testing "A multi bulk command"

    (testing "requires at least one bulk"
      (is (thrown? Exception (make-multi-bulk-command))))

    (testing "should write its arguments as a multi bulk command"
      (let [c (make-multi-bulk-command "PING")]
        (is (= "*1\r\n$4\r\nPING\r\n"
               (str c))))
      (let [c (make-multi-bulk-command "GET" "akey")]
        (is (= "*2\r\n$3\r\nGET\r\n$4\r\nakey\r\n"
               (str c))))
      (let [c (make-multi-bulk-command "SET" "key" "value")]
        (is (= "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$5\r\nvalue\r\n"
               (str c)))))

    (testing "should write binary-safe strings"
      (let [c (make-multi-bulk-command "new\r\nline\0")]
        (is (= "*1\r\n$10\r\nnew\r\nline\0\r\n"
               (str c)))))))

(defn- wrap-in-stream [s]
  (let [bytes (.getBytes s "ASCII")
        buf (ByteArrayInputStream. bytes)
        stream (BufferedInputStream. buf)]
    stream))

(defn- get-reply [s]
  (let [stream (wrap-in-stream s)]
    (read-reply stream)))

(deftest read-reply-test
  (testing "Invalid replies should throw exceptions"
    (is (thrown? Exception (get-reply "")))
    (is (thrown? Exception (get-reply "#")))
    (is (thrown? Exception (get-reply "-\r\n")))
    (is (thrown? Exception (get-reply "+OK\n")))
    (is (thrown? Exception (get-reply "+OK\r"))))

  (testing "An error reply"
    (testing "should throw an exception with the error as message"
      (is (thrown-with-msg? Exception #"Error!" (get-reply "-Error!\r\n")))))

  (testing "A single line reply"
    (testing "should return the message as a string"
      (is (= "OK"
             (get-reply "+OK\r\n")))))
  
  (testing "An integer reply"
    (testing "should return the integer value")
    (is (= 42
           (get-reply ":42\r\n"))))
  
  (testing "A bulk reply"
    (testing "should return nil for negative bulk lengths"
      (is (= nil
             (get-reply "$-1\r\n"))))
    (testing "should return the bulk as a string"
      (is (= "blahonga"
             (get-reply "$8\r\nblahonga\r\n"))))
    (testing "should be able to read binary-safe strings"
      (is (= "Hello\r\nWorld!\n"
             (get-reply "$14\r\nHello\r\nWorld!\n\r\n")))))

  (testing "A multi bulk reply"
    (testing "should return nil for negative bulk counts"
      (is (= nil
             (get-reply "*-1\r\n"))))
    (testing "should return a list with all bulks"
      (is (= ["OK" 42 nil "blahonga" "Hello\r\nWorld!\n"]
             (get-reply "*5\r\n+OK\r\n:42\r\n$-1\r\n$8\r\nblahonga\r\n$14\r\nHello\r\nWorld!\n\r\n"))))))


