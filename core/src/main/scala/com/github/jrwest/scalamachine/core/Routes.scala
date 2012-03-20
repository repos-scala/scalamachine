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

sealed trait Route extends PartialFunction[Seq[String],(Resource,PathData)] {
  def pathTerms: Seq[RouteTerm]
  def resource: Resource
  
  protected def hasStar: Boolean
  
  def isDefinedAt(path: Seq[String]) = {
    buildPathData(path).isDefined
  }
  
  def apply(path: Seq[String]) =
    buildPathData(path) map { (resource, _) } getOrElse { throw new MatchError("Path doesn't match route") }


  private def buildPathData(path: Seq[String]): Option[PathData] = {
    if ((hasStar && path.size >= pathTerms.size - 1) || (pathTerms.size == path.size)) {
      val matches = (path,pathTerms).zipped.forall { (token: String, term: RouteTerm) =>
        term match {
          case StringPart(expected) => expected == token
          case _ => true
        }
      }
      val tokens = if (hasStar) path drop (pathTerms.size - 1) else Nil
      if (matches) Some(PathData(tokens = tokens)) else None
    } else None
  }

}

object Route {
  def routeMatching(terms: Seq[RouteTerm], r: => Resource): Route = new Route {
    def pathTerms: Seq[RouteTerm] = terms
    def resource: Resource = r
    protected val hasStar = false
  }

  def routeStartingWith(terms: Seq[RouteTerm], r: => Resource): Route = new Route {
    def pathTerms: Seq[RouteTerm] = terms ++ Seq(StarTerm)
    def resource: Resource = r
    protected val hasStar = true
  }
}


