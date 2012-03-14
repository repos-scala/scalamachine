package com.github.jrwest.scalamachine.core


// FOR LIFT CAN EXTEND PFDISPATCH LIKE RESTHELPER

/*

   in lift:
   object MyWebMachine extends LiftDispatchTable {
     addRoute(
   }
   LiftRules.statelessDispatch.append(MyWebmachine)
 */
/*
trait DispatchTable[-A,+B,W[_]] extends PartialFunction[A,W[B]] {
  protected var _routes: List[Route[Any]] = Nil
  def addRoute[C](route: Route[C]) { _routes ::= route} //{ routes ::= route }
  def isDefinedAt(req: A): Boolean = _routes.find(_.isDefinedAt(path(req))).isDefined
  // TODO: add route data to ReqRespData (see RouteData case class)
  def apply(req: A): W[B] = {
    val data = toData(req)
    wrap {
      fromData {
        _routes.find(_.isDefinedAt(path(req))).flatMap(_(data.pathParts)).map {
          flowRunner.run(firstDecision, _, data)
        } getOrElse handle404(data)
      }
    }    
  } 

  def path(req: A): List[String]
  def wrap(res: => B): W[B]
  def flowRunner: FlowRunner[Any]
  def firstDecision: Decision[Any]
  def toData(req: A): ReqRespData
  def fromData(data: ReqRespData): B
  // when no route matches
  def handle404(data: ReqRespData): ReqRespData
}

trait Route[C] extends PartialFunction[List[String],Option[Resource[C]]] {
  def parts: List[RoutePart]
  def resource: Resource[C]
  
  def isDefinedAt(path: List[String]) = false // TODO: implement me
  def apply(path: List[String]) = if (isDefinedAt(path)) Some(resource) else None
}
object Route {
  
  def routeMatching[C](prts: List[RoutePart2], res: => Resource[C]) = new Route[C] {
    def parts: List[RoutePart] = prts
    def resource = res
  }

  def routeStartingWith[C](prts: List[RoutePart2], res: => Resource[C]) = new Route[C] {
    def parts: List[RoutePart] = prts ::: List(StarRoutePart)
    def resource = res
  }
  
}
trait RoutePart
trait RoutePart2 extends RoutePart // needs better name
case class RouteString(s: String) extends RoutePart2
case class RouteData(name: Symbol) extends RoutePart2
private[core] case object StarRoutePart extends RoutePart*/
