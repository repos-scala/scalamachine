package com.github.jrwest.scalamachine.core

trait DispatchTable[-A,B,+W[_]] extends PartialFunction[A,W[B]] {

  private var _routes: List[Route] = Nil

  def addRoute(route: Route) { _routes ::= route }

  def isDefinedAt(req: A): Boolean = _routes.find(_.isDefinedAt(path(req))).isDefined

  // TODO: add route data to ReqRespData (see DataPart case class)
  def apply(req: A): W[B] = {
    val data = toData(req)
    wrap {
      fromData {        
        _routes.find(_.isDefinedAt(path(req)))
          .map(r => flowRunner.run(firstDecision, r(data.pathParts), data))
          .getOrElse(handle404(data))
      }
    }
  }

  // when no route matches
  // container implementations that support pass through will never need to call this function if they properly use DispatchTable.isDefinedAt
  def handle404(data: ReqRespData): ReqRespData = data.setStatusCode(404)

  // default flow runner
  def flowRunner = new FlowRunner

  def path(req: A): List[String]

  def wrap(res: => B): W[B]

  def firstDecision: Decision

  def toData(req: A): ReqRespData

  def fromData(data: ReqRespData): B

}

trait V3DispatchTable[-A,B,+W[_]] extends DispatchTable[A,B,W] with WebmachineDecisions {
  val firstDecision = b13
}