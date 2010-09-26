;
; A few simple benchmarks, inspired by redis-rb.
;
(ns benchmarks.ruby
  (:require redis))

(set! *warn-on-reflection* true)

(defmacro measure-time [body]
  "Call fn and return the time it took in seconds."
  `(let [start# (. System (nanoTime))]
    ~body
    (/ (- (. System (nanoTime)) start#) 1000000000.0)))


(defn ping [n]
  (print "Performing" n "PING commands: ")
  (measure-time
   (dotimes [i n]
     (redis/ping))))

(defn set [n]
  (print "Setting" n "keys: ")
  (measure-time
   (dotimes [i n]
     (let [key (str "foo" i)])
     (redis/set key "xxx"))))

(defn set-and-get [n]
  (print "Setting and getting" n "keys: ")
  (measure-time
   (dotimes [i n]
     (let [key (str "foo" i)]
       (redis/set key (apply str (repeat 10 key)))
       (redis/get key)))))

(defn set-and-get-pipelined [n]
  (print "Setting and getting" n "keys (pipelined): ")
  (measure-time
   (dotimes [i n]
     (let [key (str "foo" i)]
       (redis/pipeline
        (redis/set key (apply str (repeat 10 key)))
        (redis/get key))))))

(defn run-benchmark [fn iterations]
  (redis/with-server
    {:db 15}
    (let [elapsed (apply fn iterations nil)
          kops (* 2 (/ iterations 1000 elapsed))]
      (println (format "%.2f" kops) "KOp/s"))))

(dotimes [i 4]
  (run-benchmark ping 20000)
  (run-benchmark set 20000)
  (run-benchmark set-and-get 20000)
  (run-benchmark set-and-get-pipelined 20000))




