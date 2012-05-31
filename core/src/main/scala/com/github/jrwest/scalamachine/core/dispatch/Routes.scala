package com.github.jrwest.scalamachine.core
package dispatch

sealed trait RouteTerm extends (String => Boolean)

sealed trait RoutePart extends RouteTerm

private[core] case object StarTerm extends RouteTerm {
  def apply(pathPart: String) = true
}

case class StringPart(value: String) extends RoutePart {
  def apply(pathPart: String) = pathPart.equalsIgnoreCase(value)
}

case class DataPart(name: Symbol) extends RoutePart {
  def apply(pathPart: String) = true
}

sealed trait Route extends PartialFunction[(Seq[String],Seq[String]), (Resource, PathData, HostData)] {
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


  // TODO: lots of refactoring

  def isDefinedAt(hostAndPath: (Seq[String],Seq[String])) = {
    if (checkPath)
      buildData(hostAndPath._2, pathTerms, pathHasStar).isDefined
    else
      buildData(hostAndPath._1.reverse, hostTerms.reverse, hostHasStar).isDefined
  }

  def apply(hostAndPath: (Seq[String],Seq[String])) =
    if (checkPath) {
      buildData(hostAndPath._2, pathTerms, pathHasStar) map { r =>
        (resource, PathData(tokens = r._1, info = r._2), HostData(tokens = hostAndPath._1))
      } getOrElse {
        throw new MatchError("route doesn't match path")
      }
    } else {
      buildData(hostAndPath._1.reverse, hostTerms.reverse, hostHasStar) map { r =>
        (resource, PathData(tokens = hostAndPath._2), HostData(tokens = r._1.reverse, info = r._2))
      } getOrElse {
        throw new MatchError("route doesn't match host")
      }
    }


  private def buildData(tokens: Seq[String], terms: Seq[RouteTerm], hasStar: Boolean): Option[(Seq[String], Map[Symbol,String])] = {
    val termsLength = terms.size
    val tokensLength = tokens.size
    if ((hasStar && tokensLength >= termsLength - 1) || (termsLength == tokensLength)) {
      @annotation.tailrec
      def matchAndExtract(tkns: Stream[String], trms: Stream[RouteTerm], infoAcc: Map[Symbol, String]): (Boolean, Map[Symbol, String]) = (tkns, trms) match {
        case (Stream.Empty, _) => (true, infoAcc)
        case (_, Stream.Empty) => (true, infoAcc)
        case (tk #:: tkRest, trm #:: trmsRest) => trm match {
          case StringPart(expected) =>
            if (expected == tk) matchAndExtract(tkRest, trmsRest, infoAcc)
            else (false, infoAcc)
          case DataPart(key) => matchAndExtract(tkRest, trmsRest, infoAcc + (key -> tk))
          case _ => matchAndExtract(tkRest, trmsRest, infoAcc)
        }
      }
      val (matches, pathInfo) = matchAndExtract(tokens.toStream, terms.toStream, Map())
      val dispTokens = if (hasStar) tokens drop (termsLength - 1) else Nil
      if (matches) Some((dispTokens, pathInfo)) else None
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

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath = true
      val checkHost = false
    }
  }

  def pathStartingWith(terms: Seq[RoutePart]) = new Serve {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = terms ++ Seq(StarTerm)
      val hostTerms: Seq[RouteTerm] = Nil

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath = true
      val checkHost = false
    }
  }

  def hostMatching(terms: Seq[RoutePart]) = new Serve {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = Nil
      val hostTerms: Seq[RouteTerm] = terms

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath: Boolean = false
      val checkHost: Boolean = true
    }
  }

  def hostEndingWith(terms: Seq[RouteTerm]) = new Serve {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = Nil
      val hostTerms: Seq[RouteTerm] = StarTerm +: terms

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath: Boolean = false
      val checkHost: Boolean = true
    }
  }
}


