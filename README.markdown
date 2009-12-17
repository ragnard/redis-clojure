# redis-clojure

A Clojure client library for the
[Redis](http://code.google.com/p/redis) key value (and more!) storage system.

## Dependencies

redis-clojure uses
[Leiningen](http://github.com/technomancy/leiningen) as build tool

## Building 

To build redis-clojure:

    lein jar

This will build redis-clojure and package it nicely into `redis-clojure.jar`.

## Running tests

To run tests:

    lein test

*Note* you need to have `redis-server` running first.

## Examples

Check the `examples/` directory.

*Note* you need to have `redis-server` running first.

## Todo

* Work on performance
* Maybe implement pipelining
* Look at consistent hashing

