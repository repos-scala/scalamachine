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

sealed trait Route extends PartialFunction[(Seq[String],Seq[String]), (Resource, PathData, PathData)] {
  def pathTerms: Seq[RouteTerm]
  def hostTerms: Seq[RouteTerm]

  def resource: Resource

  def checkPath: Boolean
  def checkHost: Boolean

  protected lazy val pathHasStar = pathTerms.reverse.headOption.map {
    case StarTerm => true
    case _ => false
  } getOrElse false

  protected lazy val hostHasStar = hostTerms.headOption.map {
    case StarTerm => true
    case _ => false
  } getOrElse false

  def isDefinedAt(hostAndPath: (Seq[String],Seq[String])) = {
    buildPathData(hostAndPath._2).isDefined
  }

  def apply(hostAndPath: (Seq[String],Seq[String])) =
    if (checkPath) {
      buildPathData(hostAndPath._2) map {
        (resource, _, PathData())
      } getOrElse {
        throw new MatchError("route doesn't match route")
      }
    } else throw new MatchError("host routes not yet supported")


  private def buildPathData(path: Seq[String]): Option[PathData] = {
    if ((pathHasStar && path.size >= pathTerms.size - 1) || (pathTerms.size == path.size)) {
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
      val tokens = if (pathHasStar) path drop (pathTerms.size - 1) else Nil
      if (matches) Some(PathData(tokens = tokens, info = pathInfo)) else None
    } else None
  }

}


object Route {

  trait Serve {
    def serve(r: => Resource): Route
  }

  def pathMatching(terms: Seq[RoutePart]) = new Serve {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = terms
      val hostTerms: Seq[RouteTerm] = Nil

      // Note: THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath = true
      val checkHost = false
    }
  }

  def pathStartingWith(terms: Seq[RoutePart]) = new Serve {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = terms ++ Seq(StarTerm)
      val hostTerms: Seq[RouteTerm] = Nil

      def resource: Resource = r

      val checkPath = true
      val checkHost = false
    }
  }
}


