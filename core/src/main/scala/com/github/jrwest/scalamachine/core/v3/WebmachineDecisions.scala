package com.github.jrwest.scalamachine.core
package v3

import flow._
import scalaz.std.option._
import scalaz.std.string._
import optionSyntax._
import scalaz.syntax.pointed._
import scalaz.syntax.order._
import scalaz.syntax.applicative._
import scalaz.OptionT._
import scalaz.Lens._
import Decision.FlowState
import Res._
import ResT._
import ReqRespData._
import Metadata._
import Resource._
import java.util.Date
import scalaz.{OptionT, State}

trait WebmachineDecisions {

  /* Service Available? */
  lazy val b13: Decision = Decision("v3b13", true, (r: Resource) => r.serviceAvailable(_: ReqRespData), b12, 503)

  /* Known Methods */
  lazy val b12: Decision = 
    Decision(
      "v3b12", 
      (r: Resource) => r.knownMethods(_: ReqRespData), 
      (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method), 
      b11, 
      501)

  /* URI Too Long? */
  lazy val b11: Decision = Decision("v3b11", true, (r: Resource) => r.uriTooLong(_: ReqRespData), 414, b10)

  /* Allowed Methods */
  lazy val b10: Decision =
    Decision(
      "v3b10",
      (r: Resource) => r.allowedMethods(_: ReqRespData),
      (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method),
      b9,
      (r: List[HTTPMethod]) => for {
          _ <- (statusCodeL := 405)
          _ <- (responseHeadersL += (("Allow" -> r.map(_.toString).mkString(", "))))
        } yield r
    )

  /* Malformed Request? */
  lazy val b9: Decision = Decision("v3b9", true, (r: Resource) => r.isMalformed(_: ReqRespData), 400, b8)

  /* Is Authorized? */
  lazy val b8: Decision = Decision(
    "v3b8",
    AuthSuccess,
    (r: Resource) => r.isAuthorized(_: ReqRespData),
    b7,
    (r: AuthResult) => for {
      _ <- r.fold(
        failure = (failMsg: String) => (responseHeadersL += ("WWW-Authenticate" -> failMsg)),
        success = responseHeadersL.st
      )
      _ <- (statusCodeL := 401)
    } yield r
  )

  /* Is Forbidden? */
  lazy val b7: Decision = Decision("v3b7", true, (r: Resource) => r.isForbidden(_: ReqRespData), 403, b6)

  /* Content-* Headers Are Valid? */
  lazy val b6: Decision = Decision("v3b6", true, (r: Resource) => r.contentHeadersValid(_: ReqRespData), b5, 501)

  /* Is Known Content-Type? */
  lazy val b5: Decision = Decision("v3b5", true, (r: Resource) => r.isKnownContentType(_: ReqRespData), b4, 415)

  /* Request Entity Too Large? */
  lazy val b4: Decision = Decision("v3b4", true, (r: Resource) => r.isValidEntityLength(_: ReqRespData), b3, 413)

  /* OPTIONS? */
  lazy val b3: Decision = new Decision {
    val name: String = "v3b3"
    val default = HaltRes(200)
    protected def decide(resource: Resource): State[ReqRespData, Res[Decision]] = {
      def handle(method: HTTPMethod): State[ReqRespData,Res[Decision]] = method match {
        case OPTIONS => {
          val set = for {
            hdrs <- resT[FlowState](State((d: ReqRespData) => resource.options(d)))
            _ <- resT[FlowState]((responseHeadersL ++= hdrs.toList).map(_.point[Res]))
            _ <- resT[FlowState](State((d: ReqRespData) => (halt[Decision](200), d)))
          } yield c3 // we will never get here
          set.run
        }
        case _ => c3.point[Res].point[FlowState]
      }
      methodL flatMap { handle(_) }
    }
  }

  /* Accept Exists? */
  lazy val c3: Decision = new Decision {
    import scalaz.syntax.std.allV.ToListVFromList

    // TODO: move somewhere more global it will probably be used elsewhere
    val defaultContentType = ContentType("text/plain")

    val name: String = "v3c3"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      performDecision(resource).map(_.point[Res])
    }

    private def performDecision(resource: Resource): State[ReqRespData,Decision] = for {
        mbAcceptHeader <- requestHeadersL member "accept"
        decision <- if (mbAcceptHeader.isDefined) State((c4, _: ReqRespData)) else resolveContentType(resource)
      } yield decision


    private def resolveContentType(r: Resource): State[ReqRespData,Decision] = for {
        res <- State[ReqRespData,Res[ContentTypesProvided]](s => r.contentTypesProvided(s))
        cType <- State((s: ReqRespData) => (firstOrDefault(res),s))
        _ <- (metadataL <=< contentTypeL) := Option(cType)
      } yield d4 

    private def firstOrDefault(res: Res[ContentTypesProvided]): ContentType =  
      (res map { (_: ContentTypesProvided).toNel.map(_.head._1) getOrElse defaultContentType }) | defaultContentType            
  }

  /* Acceptable Media Type Available? */
  lazy val c4: Decision = new Decision {
    val name: String = "v3c4"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      for {
        acceptHeader <- ((requestHeadersL member "accept") map { _ getOrElse "*/*" })
        providedResult <- State((d: ReqRespData) => resource.contentTypesProvided(d))
        provided <- (providedResult getOrElse Nil).unzip._1.point[FlowState]
        contentType <- Util.chooseMediaType(provided, acceptHeader).point[FlowState]
        _ <- (metadataL <=< contentTypeL) := contentType
      } yield contentType >| d4.point[Res] | HaltRes(406)
    }
  }

  /* Accept-Language Exists? */
  lazy val d4: Decision = new Decision {
    val name = "v3d4" 

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      (requestHeadersL member "accept-language").map(_ >| d5.point[Res] | e5.point[Res])
    }
  }

  /* Acceptable Language Available? */
  lazy val d5: Decision = Decision("v3d5", true, (r: Resource) => r.isLanguageAvailable(_: ReqRespData), e5, 406)

  /* Accept-Charset Exists? */
  lazy val e5: Decision = new Decision {
    def name: String = "v3e5"


    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        mbHeader <- resT[FlowState]((requestHeadersL member "accept-charset") map { _.point[Res] })
        decision <- mbHeader >| resT[FlowState](result(e6).point[FlowState]) | chooseCharset(resource, "*")
      } yield decision
      act.run
    }

  }

  /* Acceptable Charset Available? */
  lazy val e6: Decision = new Decision {
    def name: String = "v3e6"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        // we can assume that the getOrElse case will never be run because of e5 but if it is the case
        // star will still be handled appropriately
        header <- resT[FlowState]((requestHeadersL member "accept-charset") map { _.getOrElse("*").point[Res] })
        decision <- chooseCharset(resource, header)
      } yield decision

      act.run
    }
  }

  /* Accept-Encoding Exists? */
  lazy val f6: Decision = new Decision {
    def name: String = "v3f6"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =  {
      val act = for {
        media <- resT[FlowState]((metadataL <=< contentTypeL).map(_.getOrElse(ContentType("text/plain")).point[Res]))
        charset <- resT[FlowState]((metadataL <=< chosenCharsetL).map(_.map(";charset=" + _).getOrElse("").point[Res]))
        _ <- resT[FlowState](((responseHeadersL member "content-type") := some(media.toHeader + charset)).map(_.point[Res]))
        mbHeader <- resT[FlowState]((requestHeadersL member "accept-encoding") map { _.point[Res] })
        decision <- mbHeader >| resT[FlowState](result(f7).point[FlowState]) | chooseEncoding(resource, "identity;q=1.0,*;q=0.5")
      } yield decision

      act.run
    }
  }

  /* Acceptable Encoding Available? */
  lazy val f7: Decision = new Decision {
    def name: String = "v3f7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        // like e6 we can assume we have already tested the default case and we won't ever run if we get here
        header <- resT[FlowState]((requestHeadersL member "accept-encoding") map { _.getOrElse("identity;q=1.0,*;q=0.5").point[Res] })
        decision <- chooseEncoding(resource, header)
      } yield decision

      act.run
    }

  }

  /* Resource Exists? */
  lazy val g7: Decision = new Decision {
    def name: String = "v3g7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val variances: ResT[FlowState,String] = for {
        variances <- resT[FlowState](State((d: ReqRespData) => resource.variances(d)))
        ctypes <- resT[FlowState](State((d: ReqRespData) => resource.contentTypesProvided(d)))
        charsets <- resT[FlowState](State((d: ReqRespData) => resource.charsetsProvided(d)))
        encodings <- resT[FlowState](State((d: ReqRespData) => resource.encodingsProvided(d)))
      } yield {
        val defaults = List(
          (ctypes.length, "Accept"),
          (charsets.getOrElse(Nil).length, "Accept-Charset"),
          (encodings.getOrElse(Nil).length, "Accept-Encoding"))
        ((defaults filter { _._1 > 1}).unzip._2 ++ variances).mkString(",")
      }
      val act = for {
        vary <- variances
        _ <- resT[FlowState]((responseHeadersL += (("vary", vary))) map { _.point[Res] })
        resourceExists <- resT[FlowState](State((d: ReqRespData) => resource.resourceExists(d)))
      } yield if (resourceExists) g8 else h7
      act.run
    }
  }

  /* If-Match Exists? */
  lazy val g8: Decision = new Decision {
    def name: String = "v3g8"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists("if-match", result(g9), result(h10))

  }

  /* If-Match: *? */
  lazy val g9: Decision = new Decision {
    def name: String = "v3g9"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbHeader <- (requestHeadersL member "if-match") map { _ filterNot { _ === "*"} }
    } yield mbHeader >| result(g11) | result(h10)

  }

  /* Etag in If-Match? */
  lazy val g11: Decision =
    Decision(
      "v3g11",
      (r: Resource) => r.generateEtag(_: ReqRespData),
      (etag: Option[String], d: ReqRespData) => (for {
        e <- optionT[FlowState](etag.point[FlowState])
        matches <- optionT[FlowState]((requestHeadersL member "if-match"))
      } yield matches.split(",").map(_.trim).toList.contains(e)) getOrElse false eval d,
      h10,
      412
    )

  /* If-Match Exists? - note: this differs from v3 diagram but follows erlang implementation */
  lazy val h7: Decision = new Decision {
    def name: String = "v3h7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists("if-match", result(i7), halt(412))
  }

  /* If-Unmodified-Since Exists? */
  lazy val h10: Decision = new Decision {
    def name: String = "v3h10"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbIums <- (requestHeadersL member "if-unmodified-since")
    } yield mbIums >| result(h11) | result(i12)
  }

  /* If-Unmodified-Since Valid Date? */
  lazy val h11: Decision = new Decision {
    def name: String = "v3h11"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
        // if we have reached here we have verified iums has value so we default
        // empty string which should never be reached
        iums <- (requestHeadersL member "if-unmodified-since").map { _ getOrElse "" }
        isValid <- (Util.parseDate(iums) >| true | false).point[FlowState]
      } yield if (isValid) result(h12) else result(i12)
  }

  /* Last-Modified > If-UnmodifiedSince? */
  lazy val h12: Decision = new Decision {
    def name: String = "v3h12"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      def isModified(mbLastMod: Option[Date], mbIums: Option[String]) =
        (mbLastMod |@| mbIums.map(Util.parseDate(_)).getOrElse(none)) { _.getTime > _.getTime } getOrElse false

      val act = for {
        // same deal here as h11
        mbIums <- resT[FlowState]((requestHeadersL member "if-unmodified-since") map { _.point[Res] })
        mbLastMod <- resT[FlowState](State((d: ReqRespData) => resource.lastModified(d)))
        decision <- resT[FlowState](if (isModified(mbLastMod, mbIums)) halt[Decision](412).point[FlowState] else result(i12).point[FlowState])
      } yield decision

      act.run
    }
  }

  /* Moved Permanently? (Apply Put to Different URI?) */
  lazy val i4: Decision =
    Decision(
      "v3i4",
      (r: Resource) => r.movedPermanently(_: ReqRespData),
      (l: Option[String], _: ReqRespData) => !l.isDefined,
      p3,
      (location: Option[String]) => for {
        _ <- (statusCodeL := 301)
        _ <- (responseHeadersL member "location") := location
      } yield location
    )

  /* PUT? (after finding resource doesn't exist) */
  lazy val i7: Decision = new Decision {
    def name: String = "v3i7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      method <- methodL
    } yield if (method === PUT) result(i4) else result(k7)
  }

  /* If-None-Match Exists? */
  lazy val i12: Decision = new Decision {
    def name: String = "v3i12"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists("if-none-match", result(i13), result(l13))
  }

  /* If-None-Match: *? */
  lazy val i13: Decision = new Decision {
    def name: String = "v3i13"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbHeader <- (requestHeadersL member "if-none-match") map { _ filterNot { _ === "*"} }
    } yield mbHeader >| result(k13) | result(j18)
  }

  lazy val j18: Decision = new Decision {
    def name: String = "v3j18"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      method <- methodL
    } yield if (List(GET,HEAD).contains(method)) halt(304) else halt(412)
  }

  /* Resource Moved Permanently? (not PUT request) */
  lazy val k5: Decision = Decision(
    "v3k5",
    (r: Resource) => r.movedPermanently(_: ReqRespData),
    (l: Option[String], _: ReqRespData) => !l.isDefined,
    l5,
    (location: Option[String]) => for {
      _ <- (statusCodeL := 301)
      _ <- (responseHeadersL member "location") := location
    } yield location
  )

  /* Resourece Existed Previously ? */
  lazy val k7: Decision = Decision("v3k7", true, (r: Resource) => r.previouslyExisted(_: ReqRespData), k5, l7)

  lazy val k13: Decision =
    Decision(
      "v3k13",
      (r: Resource) => r.generateEtag(_: ReqRespData),
      (etag: Option[String], d: ReqRespData) => (for {
        e <- optionT[FlowState](etag.point[FlowState])
        matches <- optionT[FlowState]((requestHeadersL member "if-none-match"))
      } yield matches.split(",").map(_.trim).toList.contains(e)) getOrElse false eval d,
      j18,
      l13
    )

  lazy val l5: Decision = Decision(
    "v3l5",
    (r: Resource) => r.movedTemporarily(_: ReqRespData),
    (l: Option[String], _: ReqRespData) => !l.isDefined,
    m5,
    (location: Option[String]) => for {
      _ <- (statusCodeL := 307)
      _ <- (responseHeadersL member "location") := location
    } yield location
  )

  lazy val l7: Decision = new Decision {
    def name: String = "v3l7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = null
  }

  lazy val l13: Decision = new Decision {
    def name: String = "v3l13"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = null
  }

  lazy val m5: Decision = new Decision {
    def name: String = "v3m5"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = null
  }

  lazy val p3: Decision = new Decision {
    def name: String = "v3p3"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = null
  }

  /** Helper Functions **/
  private def chooseCharset(resource: Resource, acceptHeader: String): ResT[FlowState, Decision] = {

    val charsetsProvided: ResT[FlowState,CharsetsProvided] = resT[FlowState](State((d: ReqRespData) => resource.charsetsProvided(d)))

    def doChoose(mbProvided: Resource.CharsetsProvided): Res[(Decision, Option[String])] =
      mbProvided.map { provided =>
        Util.chooseCharset(provided.unzip._1, acceptHeader)
          .map(c => result((f6, some(c))))
          .getOrElse(halt(406))
      } getOrElse { result((f6, none)) }

    def setCharsetMeta(chosen: Option[String]): ResT[FlowState,Option[String]] =
      resT[FlowState](((metadataL <=< chosenCharsetL) := chosen).map(_.point[Res]))

    for {
      p <- charsetsProvided
      (decision, chosen) <- resT[FlowState](doChoose(p).point[FlowState])
      _ <- setCharsetMeta(chosen)
    } yield decision
  }

  private def chooseEncoding(resource: Resource, headerValue: String): ResT[FlowState,Decision] = {
    val encodingsProvided: ResT[FlowState,EncodingsProvided] = resT[FlowState](State((d: ReqRespData) => resource.encodingsProvided(d)))

    def doChoose(mbProvided: Resource.EncodingsProvided): Res[(Decision, Option[String])] =
      mbProvided.map { provided =>
        Util.chooseEncoding(provided.unzip._1, headerValue: String)
          .map(e => result((g7, some(e))))
          .getOrElse(halt(406))
      } getOrElse result((g7, none))

    def setEncodingHeader(chosen: Option[String]): FlowState[Option[String]] =
      (responseHeadersL member "content-encoding") := (chosen filterNot { _ === "identity" })

    for {
      p <- encodingsProvided
      (decision, chosen) <- resT[FlowState](doChoose(p).point[FlowState])
      _ <- resT[FlowState](setEncodingHeader(chosen).map { _.point[Res] })
    } yield decision
  }

  def headerExists(header: String, exists: Res[Decision], dne: Res[Decision]) = for {
    mbIfMatch <- (requestHeadersL member header)
  } yield mbIfMatch >| exists | dne


}
