package code

import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.Http
import java.net.InetSocketAddress
import scalamachine.core.dispatch.Route._
import scalamachine.core.dispatch.StringPart
import scalamachine.finagle.{FinagleWebmachineV3, FinagleWebmachineService}
import scalamachine.core.flow.{FlowLogging, FlowRunner}
import resources.{EchoPostBodyResource, EmptyResource, UnavailableResource}

object ScalamachineExample extends FinagleWebmachineV3 {

  route {
    hostMatching {
      "jordan" dot "localhost"
    } andPathMatching {
      "unavailable" / 'id / "a"
    } serve {
      new UnavailableResource
    }
  }

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
      "echo"
    } serve new EchoPostBodyResource
  }

  route {
    hostEndingWith {
      "localhost"
    } serve new EmptyResource
  }


  override val flowRunner = new FlowRunner with FlowLogging
}

object ExampleServer extends App {

  val server =
    ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("FinagleWebmachine")
      .build(new FinagleWebmachineService(ScalamachineExample))

}