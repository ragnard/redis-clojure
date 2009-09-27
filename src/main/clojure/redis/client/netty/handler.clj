(ns redis.client.netty.handler
  (:import [org.jboss.netty.channel
            ChannelEvent
            ChannelHandlerContext
            ChannelPipelineCoverage
            ChannelStateEvent
            ExceptionEvent
            MessageEvent
            SimpleChannelHandler
            ])
  (:gen-class
   :extends org.jboss.netty.channel.SimpleChannelHandler
   :main false))


(defn -messageReceived
  [this context event]
  (println (str "Got message: " (.getMessage event))))

