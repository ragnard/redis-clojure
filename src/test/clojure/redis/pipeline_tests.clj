(ns redis.pipeline-tests
  (:require [redis])
  (:use [clojure.test]))

(defn server-fixture [f]
  (redis/with-server {:host "127.0.0.1"
                      :port 6379
                      :db 15}
    (redis/set "foo" "bar")
    (f)
    (redis/flushdb)))

(use-fixtures :each server-fixture)

(deftest pipeline
  (testing "A pipeline"
    (testing "should return a vector of replies"
      (is (= ["OK" "OK" "value1" "value2"]
             (redis/pipeline
               (redis/set "key1" "value1")
               (redis/set "key2" "value2")
               (redis/get "key1")
               (redis/get "key2")))))

    (testing "should return nil for pipelined commands"
      (redis/pipeline
       (is (= nil (redis/set "key1" "value1")))
       (is (= nil (redis/get "key1")))))

    (testing "should catch and return exceptions"
      (let [reply (redis/pipeline
                   (redis/set "key1" "value1")
                   (redis/incr "key1")
                   (redis/set "key2" "value2"))]
        (is (= "OK" (nth reply 0)))
        (is (instance? Exception (nth reply 1)))
        (is (= "OK" (nth reply 2)))))))