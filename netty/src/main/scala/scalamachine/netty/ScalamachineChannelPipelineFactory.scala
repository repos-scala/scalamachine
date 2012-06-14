package scalamachine.netty

import org.jboss.netty.channel.{Channels, ChannelPipeline, ChannelPipelineFactory}
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.execution.ExecutionHandler
import scalamachine.core.dispatch.DispatchTable
import scalamachine.internal.scalaz.Id._

class ScalamachineChannelPipelineFactory(private val execHandler: ExecutionHandler, dispatchTable: DispatchTable[HttpRequest, NettyHttpResponse, Id])
  extends ChannelPipelineFactory {

  def getPipeline: ChannelPipeline = {
    val pipeline = Channels.pipeline()

    pipeline.addLast("request-decoder", new HttpRequestDecoder)
    pipeline.addLast("chunk-aggregator", new HttpChunkAggregator(1048576)) // not handling streaming requests yet
    pipeline.addLast("response-encoder", new HttpResponseEncoder)
    pipeline.addLast("execution-handler", execHandler)
    pipeline.addLast("requst-handler", new ScalamachineRequestHandler(dispatchTable))


    pipeline
  }

}