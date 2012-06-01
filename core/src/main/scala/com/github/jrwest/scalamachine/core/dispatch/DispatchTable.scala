package com.github.jrwest.scalamachine.core
package dispatch

import flow._

trait DispatchTable[-A, B, +W[_]] extends PartialFunction[A, W[B]] {

  private var _routes = Vector.empty[Route]

  def addRoute(route: Route) {
    _routes :+= route
  }

  def isDefinedAt(req: A): Boolean = _routes.find(_.isDefinedAt(host(req) -> path(req))).isDefined

  def apply(req: A): W[B] = {
    val data = toData(req)
    wrap {
      fromData {
        _routes.find(_.isDefinedAt(data.hostParts -> data.pathParts))
          .map(route => {
          val (resource, pathData, hostData) = route(data.hostParts -> data.pathParts)
          flowRunner.run(firstDecision, resource, data.setPathData(pathData).setHostData(hostData))
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

  def path(req: A): List[String]

  protected def hostString(req: A): String

  // the HOST (excluding port) split by "."
  def host(req: A): List[String] = {
    val fullName = hostString(req)
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