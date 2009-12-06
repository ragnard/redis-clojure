(require 'tests 
         'tests.internal) 

(clojure.test/run-tests 'redis.tests
                        'redis.tests.internal)