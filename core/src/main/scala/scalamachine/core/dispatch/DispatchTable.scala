package scalamachine.core
package dispatch

import flow._

trait Dispatch {
  def perform(route: Route, resource: Resource, data: ReqRespData): ReqRespData
}

trait DispatchTable[-A, B, +W[_]] extends Dispatch {

  private var _routes = Vector.empty[Route]

  def route(route: Route) {
    _routes :+= route
  }

  def routes(routes: Route*) {
    _routes ++= Vector(routes:_*)
  }

  def apply(req: A): Option[W[B]] = {
    val data = toData(req)

    _routes
      .find(_.isDefinedAt(data))
      .map(
        route => {
          val (resource, finalData) = route(data)
          wrap(fromData(perform(route, resource, data)))
        }
      )
  }


  // although not used here the route is passed to this function
  // so traits stacked on this one can use the information in the route
  def perform(route: Route, resource: Resource, data: ReqRespData): ReqRespData = {
    flowRunner.run(firstDecision, resource, data)
  }

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