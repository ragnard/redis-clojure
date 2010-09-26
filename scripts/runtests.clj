;(use 'lazytest.watch)
;(start [ "src/test"])

(ns redis-clojure.runtests
  (:require lazytest.runner.console
	    lazytest.report.nested))

(doseq [sym '[redis.command-test2]]
  (println "Loading" sym)
  (require sym)
  (println "Running tests in" sym)
  (lazytest.report.nested/report
   (lazytest.runner.console/run-tests sym)))



