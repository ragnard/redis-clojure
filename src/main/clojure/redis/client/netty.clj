;(set! *compile-path* "/Users/ragge/Projects/clojure/redis-clojure/classes")
;(compile 'redis.client.netty.handler)

(ns redis.client.netty
  (:refer-clojure :exclude [send])
  (:use redis.client)
  (:import [java.net InetSocketAddress]
           [java.util.concurrent Executors]
           [org.jboss.netty.bootstrap ClientBootstrap]
           [org.jboss.netty.channel Channel ChannelFactory ChannelFuture SimpleChannelHandler]
           [org.jboss.netty.channel.socket.nio NioClientSocketChannelFactory]
           [redis.client.netty handler]
           ))

(defstruct netty-client :bootstrap :channel)

(defn connect
  [bootstrap host port]
  (let [future (.connect bootstrap (InetSocketAddress. host port))
        channel (doto future
                  (.awaitUninterruptinbly)
                  (.getChannel))]
    (if (.isSuccess future)
      channel
      (throw (Exception. (.. future getCause getMessage))))))

(defmethod make-client :netty
  [server-spec]
  (let [{:keys [host port timeout]} server-spec
        factory (NioClientSocketChannelFactory.
                 (Executors/newCachedThreadPool)
                 (Executors/newCachedThreadPool))
        bootstrap (ClientBootstrap. factory)
        handler (handler.)
        pipeline (.getPipeline bootstrap)
        channel nil]
    (do
      (doto pipeline
        (.addLast "handler" handler))
      (doto bootstrap
        (.setOption "tcpNoDelay" true)
        (.setOption "keepAlive" true))
      (struct netty-client bootstrap channel)))

  (defmethod send :netty
    [netty-client #^String command]))