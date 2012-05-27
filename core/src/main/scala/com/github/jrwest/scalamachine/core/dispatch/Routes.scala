package com.github.jrwest.scalamachine.core
package dispatch

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

sealed trait Route extends PartialFunction[Seq[String], (Resource, PathData)] {
  def pathTerms: Seq[RouteTerm]

  def resource: Resource

  protected def hasStar: Boolean

  def isDefinedAt(path: Seq[String]) = {
    buildPathData(path).isDefined
  }

  def apply(path: Seq[String]) =
    buildPathData(path) map {
      (resource, _)
    } getOrElse {
      throw new MatchError("Path doesn't match route")
    }


  private def buildPathData(path: Seq[String]): Option[PathData] = {
    if ((hasStar && path.size >= pathTerms.size - 1) || (pathTerms.size == path.size)) {
      @annotation.tailrec
      def matchAndExtract(ps: Stream[String], prts: Stream[RouteTerm], infoAcc: Map[Symbol, String]): (Boolean, Map[Symbol, String]) = (ps, prts) match {
        case (Stream.Empty, _) => (true, infoAcc)
        case (_, Stream.Empty) => (true, infoAcc)
        case (p #:: psRest, pr #:: prtsRest) => pr match {
          case StringPart(expected) =>
            if (expected == p) matchAndExtract(psRest, prtsRest, infoAcc)
            else (false, infoAcc)
          case DataPart(key) => matchAndExtract(psRest, prtsRest, infoAcc + (key -> p))
          case _ => matchAndExtract(psRest, prtsRest, infoAcc)

        }
      }
      val (matches, pathInfo) = matchAndExtract(path.toStream, pathTerms.toStream, Map())
      val tokens = if (hasStar) path drop (pathTerms.size - 1) else Nil
      if (matches) Some(PathData(tokens = tokens, info = pathInfo)) else None
    } else None
  }

}

object Route {
  def routeMatching(terms: Seq[RoutePart], r: => Resource): Route = new Route {
    def pathTerms: Seq[RouteTerm] = terms

    def resource: Resource = r

    protected val hasStar = false
  }

  def routeStartingWith(terms: Seq[RoutePart], r: => Resource): Route = new Route {
    def pathTerms: Seq[RouteTerm] = terms ++ Seq(StarTerm)

    def resource: Resource = r

    protected val hasStar = true
  }
}


