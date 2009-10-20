# redis-clojure

A Clojure client library for the
[Redis](http://code.google.com/p/redis) key value storage system.

## Dependencies

To use redis-clojure, you'll need:

* The [Clojure](http://clojure.org) programming language

## Building 

To build redis-clojure:

    ant -Dclojure.jar=/path/to/clojure.jar

This will build `redis-clojure.jar`.

## Running tests

To run tests:

    ant -Dclojure.jar=/path/to/clojure.jar test

*Note* you need to have `redis-server` running first.

## Using

To use redis-clojure in your application, simply make sure either
`redis-clojure.jar` or the contents of the `src/main/clojure`
directory is on your classpath.

## Examples

Check the `examples/` directory.

*Note* you need to have `redis-server` running first.

## Todo

* Work on performance
* Maybe implement pipelining

