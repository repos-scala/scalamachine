package com.github.jrwest.scalamachine.core
package dispatch

import flow._

trait DispatchTable[-A, B, +W[_]] extends PartialFunction[A, W[B]] {

  private var _routes: List[Route] = Nil

  def addRoute(route: Route) {
    _routes ::= route
  }

  def isDefinedAt(req: A): Boolean = _routes.find(_.isDefinedAt(path(req))).isDefined

  // TODO: add route data to ReqRespData (see DataPart case class)
  def apply(req: A): W[B] = {
    val data = toData(req)
    wrap {
      fromData {
        _routes.find(_.isDefinedAt(data.pathParts))
          .map(route => {
          val (resource, pathData) = route(data.pathParts)
          flowRunner.run(firstDecision, resource, data.setPathData(pathData))
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

  def wrap(res: => B): W[B]

  def firstDecision: Decision

  def toData(req: A): ReqRespData

  def fromData(data: ReqRespData): B

}