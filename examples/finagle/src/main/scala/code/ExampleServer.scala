package code

import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.Http
import java.net.InetSocketAddress
import com.github.jrwest.scalamachine.core.dispatch.Route._
import com.github.jrwest.scalamachine.core.dispatch.StringPart
import resources.{EmptyResource, UnavailableResource}
import com.github.jrwest.scalamachine.finagle.{FinagleWebmachineV3, FinagleWebmachineService}

object ScalamachineExample extends FinagleWebmachineV3 {

  addRoute {
    routeMatching(StringPart("unavailable") :: Nil, new UnavailableResource)
  }

  addRoute {
    routeMatching(StringPart("empty") :: Nil, new EmptyResource)
  }
}

object ExampleServer extends App {

  val server =
    ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("FinagleWebmachine")
      .build(new FinagleWebmachineService(ScalamachineExample))

}