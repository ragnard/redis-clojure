# redis-clojure

This is work in progress for a new version of redis-clojure targeting Clojure 1.2.

New/fixed in this version is:

* Connection pooling
* Binary-safe strings (using multi bulk commands)
* Better performance
* Support for pipelining using the `pipeline` macro

Planned features:

* Sharding

redis-clojure is a Clojure client library for the
[Redis](http://code.google.com/p/redis) key value (and more!) storage
system.

The goal of redis-clojure is to provide a low level interface to all
Redis commands in a Clojure idiomatic way, when possible.

## Building 

This version of redis-clojure uses
[Leiningen](http://github.com/technomancy/leiningen) as build tool.

## Running tests

To run tests:

    lein test

*Note* you need to have `redis-server` running on `localhost` at port `6379`.

