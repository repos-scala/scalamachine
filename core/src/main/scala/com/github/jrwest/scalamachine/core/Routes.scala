package com.github.jrwest.scalamachine.core


trait RouteTerm extends (String => Boolean)
trait RoutePart extends RouteTerm
private[core] case object StarTerm extends RouteTerm {
  def apply(pathPart: String) = true
}
case class StringPart(value: String) extends RoutePart {
  def apply(pathPart: String) = pathPart.equalsIgnoreCase(value)
}
case class DataPart(name: Symbol) extends RoutePart {
  def apply(pathPart: String) = true
}

sealed trait Route extends PartialFunction[List[String],Resource] {
  def pathTerms: List[RouteTerm]
  def resource: Resource
  
  protected def hasStar: Boolean
  
  def isDefinedAt(path: List[String]) = {
    if ((hasStar && path.size >= pathTerms.size - 1) || (pathTerms.size == path.size)) {
      (path,pathTerms).zipped.forall { (token: String, term: RouteTerm) =>
        term match {
          case StringPart(expected) => expected == token
          case _ => true
        }
      }
    } else false
  }
  
  def apply(path: List[String]) = 
    if (isDefinedAt(path)) resource 
    else throw new MatchError("Path doesn't not match route")

}

object Route {
  def routeMatching(terms: List[RouteTerm], r: => Resource): Route = new Route {
    def pathTerms: List[RouteTerm] = terms
    def resource: Resource = r
    protected val hasStar = false
  }

  def routeStartingWith(terms: List[RouteTerm], r: => Resource): Route = new Route {
    def pathTerms: List[RouteTerm] = terms ::: List(StarTerm)
    def resource: Resource = r
    protected val hasStar = true
  }
}


