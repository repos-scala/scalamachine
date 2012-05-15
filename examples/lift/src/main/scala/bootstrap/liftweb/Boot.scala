package bootstrap.liftweb

import net.liftweb._
import http._
import com.github.jrwest.scalamachine.lift.LiftWebmachineV3
import com.github.jrwest.scalamachine.core.dispatch._
import Route._
import code.resources.{EmptyResource, UnavailableResource}

object ScalamachineExample extends LiftWebmachineV3 {
  addRoute {
    routeMatching(StringPart("unavailable") :: Nil, new UnavailableResource)
  }
  addRoute {
    routeMatching(StringPart("empty") :: Nil, new EmptyResource)
  }
}

class Boot {

  def boot {
    LiftRules.statelessDispatch.append(ScalamachineExample)
  }

}