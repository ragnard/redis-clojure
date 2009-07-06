(ns redis.client
  (:import [java.net InetSocketAddress]
           [java.util.concurrent Executors]
           [org.jboss.netty.bootstrap ClientBootstrap]
           [org.jboss.netty.channel Channel ChannelFactory ChannelFuture]
           [org.jboss.netty.channel.socket.nio NioClientSocketChannelFactory]
           [redis.handler RedisClientHandler]
           ))

(defstruct client :bootstrap :channel)



(defn create-client
  []
  (let [factory (NioClientSocketChannelFactory.
                 (Executors/newCachedThreadPool)
                 (Executors/newCachedThreadPool))
        bootstrap (ClientBootstrap. factory)
        handler (RedisClientHandler.)
        pipeline (.getPipeline bootstrap)
        ]
    (do
      (doto pipeline
        (.addLast "handler" handler))
      (doto bootstrap
        (.setOption "tcpNoDelay" true)
        (.setOption "keepAlive" true)))))
