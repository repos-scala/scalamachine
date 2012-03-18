package bootstrap.liftweb

import net.liftweb._
import http._
import com.github.jrwest.scalamachine.lift.LiftWebmachineV3
import com.github.jrwest.scalamachine.core.Route._
import com.github.jrwest.scalamachine.core.StringPart
import code.resources.UnavailableResource

object ScalamachineExample extends LiftWebmachineV3 {
  addRoute {
    routeMatching(StringPart("a") :: Nil, new UnavailableResource)
  }
}

class Boot {

  def boot {
    LiftRules.statelessDispatch.append(ScalamachineExample)
  }

}