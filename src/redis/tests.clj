(ns redis.tests
  (:require redis)
  (:use [clojure.contrib.test-is]))


(defn server-fixture [f]
  (redis/with-server
   {:host "127.0.0.1"
    :port 6379
    :db 15}
   (redis/set "foo" "bar")
   (f)
   (redis/flushdb)))
                     
(use-fixtures :each server-fixture)

(deftest ping
  (testing "should return PONG"
           (is (= "PONG" (redis/ping)))))

(deftest set*
  (testing "should be able to set the value at a nonexistent key"
           (redis/set "bar" "foo")
           (is (= "foo" (redis/get "bar"))))
  (testing "should be able to set the value at a key"
           (redis/set "foo" "baz")
           (is (= "baz" (redis/get "foo")))))

(deftest get*
  (testing "should return nil if key does not exist"
           (is (= nil (redis/get "bar"))))
  (testing "should return the value at a key"
           (is (= "bar" (redis/get "foo")))))

(deftest getset
  (is (= nil   (redis/getset "bar" "foo")))
  (is (= "foo" (redis/get "bar")))
  (is (= "bar" (redis/getset "foo" "baz")))
  (is (= "baz" (redis/get "foo"))))

(deftest mget
  (is (= [nil] (redis/mget "bar")))
  (redis/set "bar" "baz")
  (redis/set "baz" "buz")
  (is (= ["bar"] (redis/mget "foo")))
  (is (= ["bar" "baz"] (redis/mget "foo" "bar")))
  (is (= ["bar" "baz" "buz"] (redis/mget "foo" "bar" "baz")))
  (is (= ["bar" nil "buz"] (redis/mget "foo" "bra" "baz")))
  )

(deftest setnx
  (is (= true (redis/setnx "bar" "foo")))
  (is (= "foo" (redis/get "bar")))
  (is (= false (redis/setnx "foo" "baz")))
  (is (= "bar" (redis/get "foo"))))

(deftest incr
  (is (= 1 (redis/incr "nonexistent")))
  (is (= 1 (redis/incr "foo")))
  (is (= 2 (redis/incr "foo"))))

(deftest incrby
  (is (= 42 (redis/incrby "nonexistent" 42)))
  (is (= 0 (redis/incrby "foo" 0)))
  (is (= 5 (redis/incrby "foo" 5))))

(deftest decr
  (is (= -1 (redis/decr "nonexistent")))
  (is (= -1 (redis/decr "foo")))
  (is (= -2 (redis/decr "foo"))))

(deftest decrby
  (is (= -42 (redis/decrby "nonexistent" 42)))
  (is (= 0 (redis/decrby "foo" 0)))
  (is (= -5 (redis/decrby "foo" 5))))

(deftest exists
  (is (= true (redis/exists "foo")))
  (is (= false (redis/exists "nonexistent"))))

(deftest del
  (is (= false (redis/del "nonexistent")))
  (is (= true (redis/del "foo")))
  (is (= nil  (redis/get "foo"))))

(deftest type*
  (testing "type"
           (testing "should return :none for nonexistent key"
                    (is (= :none (redis/type "nonexistent"))))
           (testing "should return :string if value at key is a string"
                    (is (= :string (redis/type "foo"))))
           (testing "should return :list if value at key is a list"
                    (redis/rpush "list" "one")
                    (is (= :list (redis/type "list"))))
           (testing "should return :set if value at key is a set"
                    (is (= :set (redis/type "set")))))
)

(deftest keys*
  (testing "should return nil if no keys matching pattern were found"
           (is (= nil (redis/keys "a*"))))
  (testing "should return a list of keys matching pattern"
           (is (= ["foo"] (redis/keys "f*")))
           (is (= ["foo"] (redis/keys "f?o")))
           (redis/set "fuu" "baz")
           (is (= #{"foo" "fuu"} (set (redis/keys "f*"))))))

(deftest randomkey
  (testing "should return a random key"
           (is (= "foo" (redis/randomkey))))
  (testing "should return the empty string if db is empty"
           (redis/flushdb)
           (is (= "" (redis/randomkey)))))

(deftest rename
  (testing "should throw exception when renaming the same key"
           (is (thrown? Exception
                        (redis/rename "foo" "foo"))))
  (testing "should throw an exception when renaming an nonexistent key"
           (is (thrown? Exception
                        (redis/rename "nonexistent" "foo"))))
  (testing "should rename a key"
           (is (= "OK" (redis/rename "foo" "bar")))
           (is (= "bar" (redis/get "bar")))
           (is (= nil (redis/get "foo"))))
  (testing "should overwrite newkey if it already exists"
           (redis/set "foo" "bar")
           (redis/set "bar" "baz")
           (is (= "OK" (redis/rename "foo" "bar")))
           (is (= "bar" (redis/get "bar")))
           (is (= nil (redis/get "foo"))))
  )

(deftest renamenx
  (testing "should throw exception when renaming the same key"
           (is (thrown? Exception
                        (redis/renamenx "foo" "foo"))))
  (testing "should throw an exception when renaming an nonexistent key"
           (is (thrown? Exception
                        (redis/renamenx "nonexistent" "foo"))))
  (testing "should rename a key if it does not already exist"
           (is (= true (redis/renamenx "foo" "bar")))
           (is (= "bar" (redis/get "bar")))
           (is (= nil (redis/get "foo"))))
  (testing "should return false if new key already exists"
           (redis/set "foo" "bar")
           (redis/set "bar" "baz")
           (is (= false (redis/renamenx "foo" "bar"))))
  )

(deftest dbsize
  (testing "should return the number of keys in the db"
           (is (= 1 (redis/dbsize)))
           (redis/del "foo")
           (is (= 0 (redis/dbsize)))))

(deftest expire
  (testing "should expire a key"
           (is (= true (redis/expire "foo" 1)))
           (Thread/sleep 2000)
           (is (= false (redis/exists "foo"))))
  (testing "should return false if key already has an expiry set"
           (redis/set "foo" "bar")
           (is (= true (redis/expire "foo" 20)))
           (is (= false (redis/expire "foo" 10))))
  (testing "should return false if key does not exist"
           (is (= false (redis/expire "nonexistent" 42))))
  )

(deftest ttl
  (testing "should return -1 if key does not exist or has no expiry set"
           (is (= -1 (redis/ttl "nonexistent")))
           (is (= -1 (redis/ttl "foo"))))
  (testing "should return the time-to-live for a key with an expiry set"
           (redis/expire "foo" 42)
           (is (< 40 (redis/ttl "foo")))))


;;
;; List commands
;;
(deftest rpush
  (testing "should throw an exception if type at key is not a list"
           (is (thrown? Exception
                        (redis/rpush "foo"))))
  (testing "should create a new list if key does not exist"
           (is (= "OK" (redis/rpush "list" "one")))
           (is (= 1 (redis/llen "list")))
           (is (= "one" (redis/lindex "list" 0)))
           (redis/del "list"))
  (testing "should append an element to the tail of a list"
           (is (= "OK" (redis/rpush "list" "one")))
           (is (= "OK" (redis/rpush "list" "two")))
           (is (= ["one" "two"] (redis/lrange "list" 0 1)))))

(deftest lpush
  (testing "should throw an exception if type at key is not a list"
           (is (thrown? Exception
                        (redis/lpush "foo"))))
  (testing "should create a new list if key does not exist"
           (is (= "OK" (redis/lpush "list" "one")))
           (is (= 1 (redis/llen "list")))
           (is (= "one" (redis/lindex "list" 0)))
           (redis/del "list"))
  (testing "should append an element to the head of a list"
           (is (= "OK" (redis/lpush "list" "one")))
           (is (= "OK" (redis/lpush "list" "two")))
           (is (= ["two" "one"] (redis/lrange "list" 0 1)))))




(deftest select
  (testing "should select another db"
           (is (= "OK" (redis/select 0)))
           (is (= nil (redis/get "akeythat_probably_doesnotexsistindb0")))))