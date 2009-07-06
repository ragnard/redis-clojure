(ns redis.handler
  (:import [org.jboss.netty.channel
            ChannelEvent
            ChannelHandlerContext
            ChannelPipelineCoverage
            ChannelStateEvent
            ExceptionEvent
            MessageEvent
            SimpleChannelHandler])
  (:gen-class
   :extends org.jboss.netty.channel.SimpleChannelHandler
   :main false
   :name redis.handler.RedisClientHandler
   ))


(defn -messageReceived
  [this context event]
  (println (str "Got message: " (.getMessage event))))

