package com.github.jrwest.scalamachine.netty

import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.execution.ExecutionHandler
import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import scalaz.Id._

class ScalamachineChannelPipelineFactory(private val execHandler: ExecutionHandler, dispatchTable: DispatchTable[HttpRequest, HttpResponse, Id]) extends ChannelPipelineFactory {

  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("request-decoder", new HttpRequestDecoder())
    pipeline.addLast("response-encoder", new HttpResponseEncoder())
    pipeline.addLast("chunk-aggregator", new HttpChunkAggregator(1048576)) // not handling streaming requests yet
    // TODO: add chunked write handler
    pipeline.addLast("execution-handler", execHandler)
    pipeline.addLast("requst-handler", new ScalamachineRequestHandler(dispatchTable))


    pipeline
  }

}