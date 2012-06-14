package code

import org.jboss.netty.bootstrap.ServerBootstrap
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory
import java.util.concurrent.Executors
import scalamachine.netty.{NettyWebmachineV3, ScalamachineChannelPipelineFactory}
import org.jboss.netty.handler.execution.{OrderedMemoryAwareThreadPoolExecutor, ExecutionHandler}
import java.net.InetSocketAddress
import scalamachine.core.dispatch.Route._
import resources.{LocalFileResource, EmptyResource, UnavailableResource}
import scalamachine.core.flow.{FlowLogging, FlowRunner}

object ScalamachineExample extends NettyWebmachineV3 {

  route {
    pathMatching {
      "unavailable"
    } serve new UnavailableResource
  }

  route {
    pathMatching {
      "empty"
    } serve  new EmptyResource
  }

  route {
    pathMatching {
      "localfile"
    } serve new LocalFileResource
  }

  override val flowRunner = new FlowRunner with FlowLogging
}

object ExampleServer extends App {

  val bootstrap = new ServerBootstrap(
    new NioServerSocketChannelFactory(
      Executors.newCachedThreadPool(),
      Executors.newCachedThreadPool()
    )
  )

  val execHandler = new ExecutionHandler(new OrderedMemoryAwareThreadPoolExecutor(16, 1048576, 1048576))
  bootstrap.setPipelineFactory(new ScalamachineChannelPipelineFactory(execHandler, ScalamachineExample))

  bootstrap.bind(new InetSocketAddress(8080))

}