package com.github.jrwest.scalamachine.core
package dispatch

sealed trait RouteTerm extends (String => Boolean)
sealed trait RoutePart extends RouteTerm

private[core] case object StarTerm extends RouteTerm {
  def apply(pathPart: String) = true
}

sealed trait StringPart extends RoutePart {
  def value: String
  def apply(pathPart: String) = pathPart.equalsIgnoreCase(value)
  override def toString = "StringPart(%s)" format value
}

object StringPart {
  def unapply(term: RouteTerm): Option[String] = term match {
    case s: StringPart => Some(s.value)
    case _ => None
  }
}

sealed trait DataPart extends RoutePart {
  def name: Symbol
  def apply(pathPart: String) = true
  override def toString = "DataPart(%s)" format name
}

object DataPart {
  def unapply(term: RouteTerm): Option[Symbol] = term match {
    case d: DataPart => Some(d.name)
    case _ => None
  }
}

sealed trait Route extends PartialFunction[ReqRespData, (Resource, PathData, HostData)] {
  def pathTerms: Seq[RouteTerm]
  def hostTerms: Seq[RouteTerm]

  def resource: Resource

  def checkPath: Boolean
  def checkHost: Boolean

  val guard: ReqRespData => Boolean = _ => true

  def isDefinedAt(data: ReqRespData) = buildResult(data).isDefined && guard(data)

  def apply(data: ReqRespData) = buildResult(data) map { r =>
    if (guard(data)) (resource, r._1, r._2)
    else throw new MatchError("guard failure")
  } getOrElse {
    throw new MatchError("route doesn't match")
  }

  private lazy val pathHasStar = pathTerms.reverse.headOption.map {
    case StarTerm => true
    case _ => false
  } getOrElse false

  private lazy val hostHasStar = hostTerms.headOption.map {
    case StarTerm => true
    case _ => false
  } getOrElse false


  private def buildResult(data: ReqRespData): Option[(PathData, HostData)] = {
    val hostTokens = data.hostParts
    val pathTokens = data.pathParts

    lazy val pathData =
      if (checkPath) buildData(pathTokens, pathTerms, pathHasStar) map { r => PathData(tokens = r._1, info = r._2) }
      else Some(PathData(tokens = pathTokens))

    lazy val hostData =
      if (checkHost) buildData(hostTokens.reverse, hostTerms.reverse, hostHasStar) map {
        r => HostData(tokens = r._1.reverse, info = r._2)
      } else Some(HostData(tokens = hostTokens))

    for {
      pd <- pathData
      hd <- hostData
    } yield (pd, hd)
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

  sealed trait RouteConjunction {
    def andPathMatching(terms: Seq[RoutePart]): Serve
    def andPathStartingWith(terms: Seq[RoutePart]): Serve
  }

  sealed trait Serve {
    def serve(r: => Resource): Route
  }

  sealed trait GuardedBy {
    def guardedBy(f: ReqRespData => Boolean): Serve
  }

  def pathMatching(terms: Seq[RoutePart]) = new Serve with GuardedBy {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = terms
      val hostTerms: Seq[RouteTerm] = Nil

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath = true
      val checkHost = false
    }

    def guardedBy(f: ReqRespData => Boolean) = new Serve {
      def serve(r: => Resource) = new Route {
        val pathTerms: Seq[RouteTerm] = terms
        val hostTerms: Seq[RouteTerm] = Nil
        override val guard = f

        def resource: Resource = r

        val checkPath = true
        val checkHost = false
      }
    }
  }

  def pathStartingWith(terms: Seq[RoutePart]) = new Serve with GuardedBy {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = terms ++ Seq(StarTerm)
      val hostTerms: Seq[RouteTerm] = Nil

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath = true
      val checkHost = false
    }

    def guardedBy(f: ReqRespData => Boolean) = new Serve {
      def serve(r: => Resource) = new Route {
        val pathTerms: Seq[RouteTerm] = terms ++ Seq(StarTerm)
        val hostTerms: Seq[RouteTerm] = Nil
        override val guard = f

        def resource: Resource = r


        val checkPath = true
        val checkHost = false
      }
    }
  }

  def hostMatching(hTerms: Seq[RoutePart]) = new Serve with RouteConjunction with GuardedBy {

    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = Nil
      val hostTerms: Seq[RouteTerm] = hTerms

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath = false
      val checkHost = true
    }

    def guardedBy(f: ReqRespData => Boolean) = new Serve {
      def serve(r: => Resource) = new Route {
        val pathTerms: Seq[RouteTerm] = Nil
        val hostTerms: Seq[RouteTerm] = hTerms
        override val guard = f

        // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
        def resource: Resource = r

        val checkPath = false
        val checkHost = true
      }
    }


    def andPathMatching(pTerms: Seq[RoutePart]) = new Serve with GuardedBy {
      def serve(r: => Resource) = new Route {
        val pathTerms = pTerms
        val hostTerms = hTerms

        def resource = r

        val checkPath = true
        val checkHost = true
      }

      def guardedBy(f: ReqRespData => Boolean) = new Serve {
        def serve(r: => Resource) = new Route {
          val pathTerms = pTerms
          val hostTerms = hTerms
          override val guard = f

          def resource = r

          val checkPath = true
          val checkHost = true
        }
      }
    }

    def andPathStartingWith(pTerms: Seq[RoutePart]) = new Serve with GuardedBy {
      def serve(r: => Resource) = new Route {
        val pathTerms = pTerms ++ Seq(StarTerm)
        val hostTerms = hTerms

        def resource = r

        val checkPath = true
        val checkHost = true
      }

      def guardedBy(f: ReqRespData => Boolean) = new Serve {
        def serve(r: => Resource) = new Route {
          val pathTerms = pTerms ++ Seq(StarTerm)
          val hostTerms = hTerms
          override val guard = f

          def resource = r

          val checkPath = true
          val checkHost = true
        }
      }
    }
  }

  def hostEndingWith(hTerms: Seq[RouteTerm]) = new Serve with RouteConjunction with GuardedBy {
    def serve(r: => Resource) = new Route {
      val pathTerms: Seq[RouteTerm] = Nil
      val hostTerms: Seq[RouteTerm] = StarTerm +: hTerms

      // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
      def resource: Resource = r

      val checkPath: Boolean = false
      val checkHost: Boolean = true
    }

    def guardedBy(f: ReqRespData => Boolean) = new Serve {
      def serve(r: => Resource) = new Route {
        val pathTerms: Seq[RouteTerm] = Nil
        val hostTerms: Seq[RouteTerm] = StarTerm +: hTerms
        override val guard = f

        // THIS MUST BE A DEF TO ENSURE THAT THE BY-NAME PARAMETER IS EVALUATED EACH TIME
        def resource: Resource = r

        val checkPath: Boolean = false
        val checkHost: Boolean = true
      }
    }

    def andPathMatching(pTerms: Seq[RoutePart]) = new Serve with GuardedBy {
      def serve(r: => Resource) = new Route {
        val pathTerms = pTerms
        val hostTerms = StarTerm +: hTerms

        def resource = r

        val checkPath = true
        val checkHost = true
      }

      def guardedBy(f: ReqRespData => Boolean) = new Serve {
        def serve(r: => Resource) = new Route {
          val pathTerms = pTerms
          val hostTerms = StarTerm +: hTerms
          override val guard = f

          def resource = r

          val checkPath = true
          val checkHost = true
        }
      }
    }

    def andPathStartingWith(pTerms: Seq[RoutePart]) = new Serve with GuardedBy {
      def serve(r: => Resource) = new Route {
        val pathTerms = pTerms ++ Seq(StarTerm)
        val hostTerms = StarTerm +: hTerms

        def resource = r

        val checkPath = true
        val checkHost = true
      }

      def guardedBy(f: ReqRespData => Boolean) = new Serve {
        def serve(r: => Resource) = new Route {
          val pathTerms = pTerms ++ Seq(StarTerm)
          val hostTerms = StarTerm +: hTerms
          override val guard = f

          def resource = r

          val checkPath = true
          val checkHost = true
        }
      }
    }

  }


  /* RoutePart Constructors & DSL */
  def routeToken(s: String): RoutePart = new StringPart {
    def value: String = s
  }

  def routeData(n: Symbol): RoutePart = new DataPart {
    def name: Symbol = n
  }

  import com.github.jrwest.scalamachine.internal.scalaz.syntax.SyntaxV

  trait RouteVectorV extends SyntaxV[Vector[RoutePart]] {
    def /(s: String): Vector[RoutePart] = dot(s)
    def /(s: Symbol): Vector[RoutePart] = dot(s)
    def dot(s: String): Vector[RoutePart] = self :+ routeToken(s)
    def dot(s: Symbol): Vector[RoutePart] = self :+ routeData(s)
  }

  trait RouteStringV extends SyntaxV[String] {
    def /(s: String): Vector[RoutePart] = dot(s)
    def /(s: Symbol): Vector[RoutePart] = dot(s)
    def dot(s: String): Vector[RoutePart] = Vector(routeToken(self), routeToken(s))
    def dot(s: Symbol): Vector[RoutePart] = Vector(routeToken(self), routeData(s))
  }

  trait RouteSymbolV extends SyntaxV[Symbol] {
    def /(s: String): Vector[RoutePart] = dot(s)
    def /(s: Symbol): Vector[RoutePart] = dot(s)
    def dot(s: String): Vector[RoutePart] = Vector(routeData(self), routeToken(s))
    def dot(s: Symbol): Vector[RoutePart] = Vector(routeData(self), routeData(s))
  }

  implicit def vectorToV(v: Vector[RoutePart]): RouteVectorV = new RouteVectorV {
    val self = v
  }

  implicit def stringToV(s: String): RouteStringV = new RouteStringV {
    val self = s
  }

  implicit def symbolToV(s: Symbol): RouteSymbolV = new RouteSymbolV {
    val self = s
  }

  implicit def stringToRouteVector(s: String): Vector[RoutePart] = Vector(routeToken(s))
  implicit def symbolToRouteVector(s: Symbol): Vector[RoutePart] = Vector(routeData(s))
}


