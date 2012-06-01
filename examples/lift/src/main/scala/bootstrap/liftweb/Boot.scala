package bootstrap.liftweb

import net.liftweb._
import http._
import com.github.jrwest.scalamachine.lift.LiftWebmachineV3
import com.github.jrwest.scalamachine.core.dispatch._
import Route._
import code.resources.{EmptyResource, UnavailableResource}

object ScalamachineExample extends LiftWebmachineV3 {
  route {
    pathMatching {
      routeToken("unavailable") :: Nil
    } serve new UnavailableResource
  }
  route {
    pathMatching {
      routeToken("empty") :: Nil
    } serve new EmptyResource
  }
}

class Boot {

  def boot {
    LiftRules.statelessDispatchTable.append(ScalamachineExample)
  }

}