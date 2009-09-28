(require 'tests 
         'tests.internal) 

(clojure.contrib.test-is/run-tests 'redis.tests
                                   'redis.tests.internal)