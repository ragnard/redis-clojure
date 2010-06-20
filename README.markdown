# redis-clojure

This is work in progress for a new version of redis-clojure targeting Clojure 1.2.

New/fixed in this version is:
* Binary-safe strings (using multi bulk commands)
* Better performance
* Support for pipelining using the `pipeline` macro

Planned features:
* Sharding
* Connection pooling

redis-clojure is a Clojure client library for the
[Redis](http://code.google.com/p/redis) key value (and more!) storage
system.

The goal of redis-clojure is to provide a low level interface to the
commands provided by Redis in a Clojure idiomatic way, when possible.

## Building 

To build redis-clojure.jar:

    mvn package

This will build redis-clojure and package it nicely into `target/redis-clojure-$VERSION.jar`.

## Running tests

To run tests:

    mvn test

*Note* you need to have `redis-server` running on `localhost` at port `6379`.
