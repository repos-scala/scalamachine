package code.resources

import com.github.jrwest.scalamachine.core.{ValueRes, ReqRespData, Resource}


class UnavailableResource extends Resource {
  override def serviceAvailable(data: ReqRespData) = (ValueRes(false),data)
}