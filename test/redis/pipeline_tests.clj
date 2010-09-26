(ns redis.pipeline-tests
  (:use [clojure.test])
  (:use [redis.core :only (with-server)]
        [redis.pipeline :only (pipeline)])
  (:require [redis.commands :as redis]))

(defn server-fixture [f]
  (with-server {:host "127.0.0.1"
                      :port 6379
                      :db 15}
    (redis/set "foo" "bar")
    (f)
    (redis/flushdb)))

(use-fixtures :each server-fixture)

(deftest pipeline-test
  (testing "A pipeline"
    (testing "should return a vector of replies"
      (is (= ["OK" "OK" "value1" "value2"]
             (pipeline
               (redis/set "key1" "value1")
               (redis/set "key2" "value2")
               (redis/get "key1")
               (redis/get "key2")))))

    (testing "should return nil for pipelined commands"
      (pipeline
       (is (= nil (redis/set "key1" "value1")))
       (is (= nil (redis/get "key1")))))

    (testing "should catch and return exceptions"
      (let [reply (pipeline
                   (redis/set "key1" "value1")
                   (redis/incr "key1")
                   (redis/set "key2" "value2"))]
        (is (= "OK" (nth reply 0)))
        (is (instance? Exception (nth reply 1)))
        (is (= "OK" (nth reply 2)))))))