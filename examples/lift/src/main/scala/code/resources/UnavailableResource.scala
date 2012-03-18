package code.resources

import com.github.jrwest.scalamachine.core.{SimpleResult, ReqRespData, Resource}


class UnavailableResource extends Resource {
  override def serviceAvailable(data: ReqRespData) = SimpleResult(false,data)
}