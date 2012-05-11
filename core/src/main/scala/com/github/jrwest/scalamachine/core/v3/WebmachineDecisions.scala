package com.github.jrwest.scalamachine.core
package v3

import flow._
import scalaz.std.option._
import scalaz.std.string._
import optionSyntax._
import scalaz.syntax.pointed._
import scalaz.syntax.order._
import scalaz.State
import Decision.FlowState
import Res._
import ResT._
import ReqRespData._
import Metadata._
import Resource._


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
        acceptHeader <- ((requestHeadersL member "accept").st map { _ getOrElse "*/*" })
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
      (requestHeadersL member "accept-language").st.map(_ >| d5.point[Res] | e5.point[Res])
    }
  }

  /* Acceptable Language Available? */
  lazy val d5: Decision = Decision("v3d5", true, (r: Resource) => r.isLanguageAvailable(_: ReqRespData), e5, 406)

  /* Accept-Charset Exists? */
  lazy val e5: Decision = new Decision {
    def name: String = "v3e5"


    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        mbHeader <- resT[FlowState]((requestHeadersL member "accept-charset").st map { _.point[Res] })
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
        header <- resT[FlowState]((requestHeadersL member "accept-charset").st map { _.getOrElse("*").point[Res] })
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
        media <- resT[FlowState]((metadataL <=< contentTypeL).st.map(_.getOrElse(ContentType("text/plain")).point[Res]))
        charset <- resT[FlowState]((metadataL <=< chosenCharsetL).st.map(_.map(";charset=" + _).getOrElse("").point[Res]))
        _ <- resT[FlowState](((responseHeadersL member "content-type") := some(media.toHeader + charset)).map(_.point[Res]))
        mbHeader <- resT[FlowState]((requestHeadersL member "accept-encoding").st map { _.point[Res] })
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
        header <- resT[FlowState]((requestHeadersL member "accept-encoding").st map { _.getOrElse("identity;q=1.0,*;q=0.5").point[Res] })
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
      State((d: ReqRespData) => ((d.requestHeader("if-match") >| g9 | h10).point[Res], d))

  }

  /* If-Match: *? */
  lazy val g9: Decision = new Decision {
    def name: String = "v3g9"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = State((d: ReqRespData) => {
      ((d.requestHeader("if-match").filterNot(_ === "*") >| g11 | h10).point[Res], d)
    })
  }

  /* Etag in If-Match? */
  lazy val g11: Decision =
    Decision(
      "v3g11",
      (r: Resource) => r.generateEtag(_: ReqRespData),
      (etag: Option[String], d: ReqRespData) => (for {
        e <- etag
        matches <- d.requestHeader("if-match")
      } yield matches.split(",").map(_.trim).toList.contains(e)) getOrElse false,
      h10,
      412
    )

  /* If-Match Exists? - note: this differs from v3 diagram but follows erlang implementation */
  lazy val h7: Decision = new Decision {
    def name: String = "v3h7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      State((d: ReqRespData) => (d.requestHeader("if-match") >| result(i7) | halt(412), d))
  }

  lazy val h10: Decision = new Decision {
    def name: String = "v3h10"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      State((d: ReqRespData) => ((d.requestHeader("if-modified-since") >| h11 | i12).point[Res], d))
  }

  lazy val h11: Decision = new Decision {
    def name: String = "v3h11"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = null
  }

  lazy val i7: Decision = new Decision {
    def name: String = "v3i7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = null
  }

  lazy val i12: Decision = new Decision {
    def name: String = "v3i12"

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

}
