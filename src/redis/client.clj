(ns redis.client
  (:import [java.net InetSocketAddress]
           [java.util.concurrent Executors]
           [org.jboss.netty.bootstrap ClientBootstrap]
           [org.jboss.netty.channel Channel ChannelFactory ChannelFuture]
           [org.jboss.netty.channel.socket.nio NioClientSocketChannelFactory]
           ;[redis.client RedisClientHandler]
           ;[redis handler]
           ))

(defstruct client :bootstrap :channel)


(defn create-client
  []
  (let [factory (NioClientSocketChannelFactory.
                 (Executors/newCachedThreadPool)
                 (Executors/newCachedThreadPool))
        bootstrap (ClientBootstrap. factory)
        handler nil]))
