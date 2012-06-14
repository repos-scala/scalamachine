package code.resources

import scalamachine.core.{ValueRes, ReqRespData, Resource}


class UnavailableResource extends Resource {
  override def serviceAvailable(data: ReqRespData) = (data, ValueRes(false))
}
