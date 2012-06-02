package com.github.jrwest.scalamachine.core
package dispatch

import flow._

trait DispatchTable[-A, B, +W[_]] extends PartialFunction[A, W[B]] {

  private var _routes = Vector.empty[Route]

  def route(route: Route) {
    _routes :+= route
  }

  def routes(routes: Route*) {
    _routes ++= Vector(routes:_*)
  }

  def isDefinedAt(req: A): Boolean = _routes.find(_.isDefinedAt(toData(req))).isDefined

  def apply(req: A): W[B] = {
    val data = toData(req)
    wrap {
      fromData {
        _routes.find(_.isDefinedAt(data))
          .map(route => {
          val (resource, finalData) = route(data)
          flowRunner.run(firstDecision, resource, finalData)
        })
          .getOrElse(handle404(data))
      }
    }
  }

  // when no route matches
  // container implementations that support pass through will never need to call this function if they properly use DispatchTable.isDefinedAt
  def handle404(data: ReqRespData): ReqRespData = data.setStatusCode(404)

  // default flow runner
  def flowRunner = new FlowRunner

  // the HOST (excluding port) split by "."
  protected def host(fullName: String): List[String] = {
    val portStartIdx = fullName indexOf ":"
    val name =
      if (portStartIdx >= 0) fullName dropRight (fullName.length - portStartIdx)
      else fullName

    name.split("\\.").toList
  }

  def wrap(res: => B): W[B]

  def firstDecision: Decision

  def toData(req: A): ReqRespData

  def fromData(data: ReqRespData): B

}