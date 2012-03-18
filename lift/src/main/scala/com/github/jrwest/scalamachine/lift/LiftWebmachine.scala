package com.github.jrwest.scalamachine.lift

import com.github.jrwest.scalamachine.core._
import net.liftweb.http.{InMemoryResponse, Req, LiftResponse}
import net.liftweb.common.{Full, Box}


trait LiftWebmachine {
  this: DispatchTable[Req, Box[LiftResponse], Function0] => 

  def path(req: Req): List[String] = req.path.partPath

  def wrap(res: => Box[LiftResponse]): () => Box[LiftResponse] = () => res  

  def toData(req: Req): ReqRespData = ImmutableReqRespData(method = parseMethod(req.request.method))

  def fromData(data: ReqRespData): Box[LiftResponse] = Full(InMemoryResponse(
    data = Array(), // FIXME
    headers = Nil, // FIXME
    cookies = Nil,
    code = data.statusCode
  ))

  private def parseMethod(methodStr: String): HTTPMethod = HTTPMethod.fromString(methodStr)

}

trait LiftWebmachineV3 extends V3DispatchTable[Req,Box[LiftResponse],Function0] with LiftWebmachine