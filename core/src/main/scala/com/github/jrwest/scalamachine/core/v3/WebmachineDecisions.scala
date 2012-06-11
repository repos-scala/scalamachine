package com.github.jrwest.scalamachine
package core
package v3

import flow._
import scalaz.std.option._
import scalaz.std.string._
import optionSyntax._
import scalaz.syntax.order._
import scalaz.syntax.monad._
import scalaz.OptionT._
import scalaz.Lens._
import scalaz.State
import scalaz.Id
import internal.ext._
import Decision.FlowState
import Res._
import com.github.jrwest.scalamachine.core.{ResTransformer => ResT}
import ResTransformer._
import ReqRespData._
import Metadata._
import Resource._
import java.util.Date
import HTTPHeaders._
import HTTPMethods._

trait WebmachineDecisions {

  type ResTFlow[X] = ResT[FlowState,X]

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
          _ <- (responseHeadersL += ((Allow -> r.map(_.toString).mkString(", "))))
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
        failure = (failMsg: String) => (responseHeadersL += (WWWAuthenticate -> failMsg)),
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
    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      def handle(method: HTTPMethod): FlowState[Res[Decision]] = method match {
        case OPTIONS => {
          val set = for {
            hdrs <- resT[FlowState]((resource.options(_: ReqRespData)).st)
            _ <- (responseHeadersL ++= hdrs.toList).liftM[ResT]
            _ <- resT[FlowState](halt[Decision](200).point[FlowState])
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
    import scalaz.syntax.std.list._

    // TODO: move somewhere more global it will probably be used elsewhere
    val defaultContentType = ContentType("text/plain")

    val name: String = "v3c3"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      performDecision(resource).map(_.point[Res])
    }

    private def performDecision(resource: Resource): State[ReqRespData,Decision] = for {
        mbAcceptHeader <- requestHeadersL member Accept
        decision <- if (mbAcceptHeader.isDefined) c4.point[FlowState] else resolveContentType(resource)
      } yield decision


    private def resolveContentType(r: Resource): State[ReqRespData,Decision] = for {
        res <- (r.contentTypesProvided(_: ReqRespData)).st
        cType <- firstOrDefault(res).point[FlowState]
        _ <- (metadataL >=> contentTypeL) := Option(cType)
      } yield d4 

    private def firstOrDefault(res: Res[ContentTypesProvided]): ContentType =  
      (res map { (_: ContentTypesProvided).toNel.map(_.head._1) getOrElse defaultContentType }) | defaultContentType            
  }

  /* Acceptable Media Type Available? */
  lazy val c4: Decision = new Decision {
    val name: String = "v3c4"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      for {
        acceptHeader <- ((requestHeadersL member Accept) map { _ getOrElse "*/*" })
        providedResult <- (resource.contentTypesProvided(_: ReqRespData)).st
        provided <- (providedResult getOrElse Nil).unzip._1.point[FlowState]
        contentType <- Util.chooseMediaType(provided, acceptHeader).point[FlowState]
        _ <- (metadataL >=> contentTypeL) := contentType
      } yield contentType >| d4.point[Res] | HaltRes(406)
    }
  }

  /* Accept-Language Exists? */
  lazy val d4: Decision = new Decision {
    val name = "v3d4" 

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      (requestHeadersL member AcceptLanguage).map(_ >| d5.point[Res] | e5.point[Res])
    }
  }

  /* Acceptable Language Available? */
  lazy val d5: Decision = Decision("v3d5", true, (r: Resource) => r.isLanguageAvailable(_: ReqRespData), e5, 406)

  /* Accept-Charset Exists? */
  lazy val e5: Decision = new Decision {
    def name: String = "v3e5"


    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        mbHeader <- (requestHeadersL member AcceptCharset).st.liftM[ResT]
        decision <- mbHeader >| e6.point[ResTFlow] | chooseCharset(resource, "*")
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
        header <- (requestHeadersL member AcceptCharset).map(_ | "*").liftM[ResT]
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
        media <- (metadataL >=> contentTypeL).map(_ | ContentType("text/plain")).liftM[ResT]
        charset <- (metadataL >=> chosenCharsetL).map(_.map(";charset=" + _).getOrElse("")).liftM[ResT]
        _ <- ((responseHeadersL member ContentTypeHeader) := some(media.toHeader + charset)).liftM[ResT]
        mbHeader <- (requestHeadersL member AcceptEncoding).st.liftM[ResT]
        decision <- mbHeader >| f7.point[ResTFlow] | chooseEncoding(resource, "identity;q=1.0,*;q=0.5")
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
        header <- (requestHeadersL member AcceptEncoding).map(_ | "identity;q=1.0,*;q=0.5").liftM[ResT]
        decision <- chooseEncoding(resource, header)
      } yield decision

      act.run
    }

  }

  /* Resource Exists? */
  lazy val g7: Decision = new Decision {
    def name: String = "v3g7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val variances = for {
        variances <- resT[FlowState]((resource.variances(_: ReqRespData)).st)
        ctypes <- resT[FlowState]((resource.contentTypesProvided(_: ReqRespData)).st)
        charsets <- resT[FlowState]((resource.charsetsProvided(_: ReqRespData)).st)
        encodings <- resT[FlowState]((resource.encodingsProvided(_: ReqRespData)).st)
      } yield {
        val defaults = List(
          (ctypes.length, "Accept"),
          (charsets.getOrElse(Nil).length, "Accept-Charset"),
          (encodings.getOrElse(Nil).length, "Accept-Encoding"))
        ((defaults filter { _._1 > 1}).unzip._2 ++ variances).mkString(",")
      }

      val act = for {
        vary <- variances
        _ <- (responseHeadersL += ((Vary, vary))).liftM[ResT]
        resourceExists <- resT[FlowState]((resource.resourceExists(_: ReqRespData)).st)
      } yield if (resourceExists) g8 else h7

      act.run
    }
  }

  /* If-Match Exists? */
  lazy val g8: Decision = new Decision {
    def name: String = "v3g8"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists(IfMatch, result(g9), result(h10))

  }

  /* If-Match: *? */
  lazy val g9: Decision = new Decision {
    def name: String = "v3g9"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbHeader <- (requestHeadersL member IfMatch) map { _ filterNot { _ === "*"} }
    } yield mbHeader >| result(g11) | result(h10)

  }

  /* Etag in If-Match? */
  lazy val g11: Decision =
    Decision(
      "v3g11",
      (r: Resource) => r.generateEtag(_: ReqRespData),
      (etag: Option[String], d: ReqRespData) => (for {
        e <- optionT[FlowState](etag.point[FlowState])
        matches <- optionT[FlowState]((requestHeadersL member IfMatch))
      } yield matches.split(",").map(_.trim).toList.contains(e)) getOrElse false eval d,
      h10,
      412
    )

  /* If-Match Exists? - note: this differs from v3 diagram but follows erlang implementation */
  lazy val h7: Decision = new Decision {
    def name: String = "v3h7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists(IfMatch, halt(412), result(i7))
  }

  /* If-Unmodified-Since Exists? */
  lazy val h10: Decision = new Decision {
    def name: String = "v3h10"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbIums <- (requestHeadersL member IfUnmodifiedSince)
    } yield mbIums >| result(h11) | result(i12)
  }

  /* If-Unmodified-Since Valid Date? */
  lazy val h11: Decision = new Decision {
    def name: String = "v3h11"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      validateDate(IfUnmodifiedSince, result(h12), result(i12))
  }

  /* Last-Modified > If-UnmodifiedSince? */
  lazy val h12: Decision = new Decision {
    def name: String = "v3h12"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testDate(resource, IfUnmodifiedSince, halt(412), result(i12)) { _ > _ }
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
        _ <- (responseHeadersL member Location) := location
      } yield location
    )

  /* PUT? (after finding resource doesn't exist) */
  lazy val i7: Decision = new Decision {
    def name: String = "v3i7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(PUT, result(i4), result(k7))
  }

  /* If-None-Match Exists? */
  lazy val i12: Decision = new Decision {
    def name: String = "v3i12"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists(IfNoneMatch, result(i13), result(l13))
  }

  /* If-None-Match: *? */
  lazy val i13: Decision = new Decision {
    def name: String = "v3i13"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbHeader <- (requestHeadersL member IfNoneMatch) map { _ filterNot { _ === "*"} }
    } yield mbHeader >| result(k13) | result(j18)
  }

  lazy val j18: Decision = new Decision {
    def name: String = "v3j18"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(List(GET,HEAD), halt(304), halt(412))
  }

  /* Resource Moved Permanently? (not PUT request) */
  lazy val k5: Decision = Decision(
    "v3k5",
    (r: Resource) => r.movedPermanently(_: ReqRespData),
    (l: Option[String], _: ReqRespData) => !l.isDefined,
    l5,
    (location: Option[String]) => for {
      _ <- (statusCodeL := 301)
      _ <- (responseHeadersL member Location) := location
    } yield location
  )

  /* Resource Existed Previously ? */
  lazy val k7: Decision = Decision("v3k7", true, (r: Resource) => r.previouslyExisted(_: ReqRespData), k5, l7)

  lazy val k13: Decision =
    Decision(
      "v3k13",
      (r: Resource) => r.generateEtag(_: ReqRespData),
      (etag: Option[String], d: ReqRespData) => (for {
        e <- optionT[FlowState](etag.point[FlowState])
        matches <- optionT[FlowState]((requestHeadersL member IfNoneMatch))
      } yield matches.split(",").map(_.trim).toList.contains(e)) getOrElse false eval d,
      j18,
      l13
    )

  /* Moved Temporarily? */
  lazy val l5: Decision = Decision(
    "v3l5",
    (r: Resource) => r.movedTemporarily(_: ReqRespData),
    (l: Option[String], _: ReqRespData) => !l.isDefined,
    m5,
    (location: Option[String]) => for {
      _ <- (statusCodeL := 307)
      _ <- (responseHeadersL member Location) := location
    } yield location
  )

  /* POST? (after determining resource d.n.e) */
  lazy val l7: Decision = new Decision {
    def name: String = "v3l7"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(POST, result(m7), halt(404))
  }

  /* If-Modified-Since Exists? */
  lazy val l13: Decision = new Decision {
    def name: String = "v3l13"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      headerExists(IfModifiedSince, result(l14), result(m16))
  }

  /* If-Modified-Since Valid Date? */
  lazy val l14: Decision = new Decision {
    def name: String = "v3l14"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      validateDate(IfModifiedSince, result(l15), result(m16))
  }

  /* If-Modified-Since in Future? */
  lazy val l15: Decision = new Decision {
    def name: String = "v3l15"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      // since we have already validated the date, in the off chance something gets messed
      // up we handle an invalid date here and proceed accordingly
      headerDate <- (requestHeadersL member IfModifiedSince) map { _ getOrElse "" }
      inFuture <- Util.parseDate(headerDate).map(_.getTime > System.currentTimeMillis).getOrElse(true).point[FlowState]
    } yield if (inFuture) result(m16) else result(l17)
  }

  /* Last Modified > If-Modified-Since */
  lazy val l17: Decision = new Decision {
    def name: String = "v3l17"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testDate(resource, IfModifiedSince, result(m16), halt(304)){ _ > _ }
  }

  /* POST? */
  lazy val m5: Decision = new Decision {
    def name: String = "v3m5"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(POST, result(n5), halt(410))
  }

  /* Allow Missing Post? */
  lazy val m7: Decision =
    Decision(
      "v3m7",
      true,
      (r: Resource) => r.allowMissingPost(_: ReqRespData),
      n11,
      404
    )

  /* DELETE? */
  lazy val m16: Decision = new Decision {
    def name: String = "v3m16"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(DELETE, result(m20), result(n16))
  }

  /* Delete Enacted? */
  lazy val m20: Decision =
    Decision(
      "v3m20",
      true,
      (r: Resource) => r.deleteResource(_: ReqRespData),
      m20b,
      500
    )

  /* Delete Enacted? */
  lazy val m20b: Decision =
    Decision(
      "v3m20b",
      true,
      (r: Resource) => r.deleteCompleted(_: ReqRespData),
      o20,
      202
    )

  /* Resource allows POST to missing resource */
  lazy val n5: Decision =
    Decision(
      "v3n5",
      true,
      (r: Resource) => r.allowMissingPost(_: ReqRespData),
      n11,
      410
    )

  /* Redirect? (also handle POST requests here) */
  lazy val n11: Decision = new Decision {
    def name: String = "v3n11"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {

      val processPost: ResT[FlowState,Unit] = for {
        processedOk <- resT[FlowState]((resource.processPost(_: ReqRespData)).st)
        _ <- if (processedOk) encodeBodyIfSet(resource).liftM[ResT]
             else resT[FlowState](error[Decision]("failed to process post").point[FlowState])
      } yield ()

      val createPath = for {
        mbCreatePath <- resT[FlowState]((resource.createPath(_: ReqRespData)).st)
        createPath <- resT[FlowState](mbCreatePath.fold(
          some = result(_),
          none = error("create path returned none")).point[FlowState]
        )

        // set dispatch path to new path
        _ <- (dispPathL := createPath).liftM[ResT]

        // set location header if its not already set
        mbExistingLoc <- (responseHeadersL member Location).st.liftM[ResT]
        baseUri <- baseUriL.st.liftM[ResT]
        path <- pathL.st.liftM[ResT]
        _ <- mbExistingLoc
              .map(_ => (responseHeadersL member Location).st)
              .getOrElse(((responseHeadersL member Location) := Some(List(baseUri,path,createPath).mkString("/"))))
              .liftM[ResT]

        _ <- acceptContent(resource)

      } yield ()

      val act = for {
        postIsCreate <- resT[FlowState]((resource.postIsCreate(_: ReqRespData)).st)
        _ <- if (postIsCreate) createPath else processPost
        doRedirect <- doRedirectL.st.liftM[ResT]
        mbLoc <- (responseHeadersL member Location).st.liftM[ResT]
        decision <-
          if (doRedirect)
            mbLoc
              .map(_ => resT[FlowState](halt[Decision](303).point[FlowState]))
              .getOrElse(resT[FlowState](error[Decision]("redirect with no location").point[FlowState]))
          else p11.point[ResTFlow]
      } yield decision

      act.run
    }
  }

  /* POST? */
  lazy val n16: Decision = new Decision {
    def name: String = "v3n16"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(POST,result(n11),result(o16))
  }

  /* Is Conflict? (PUT requests are also handled here) */
  lazy val o14: Decision = new Decision {
    def name: String = "v3o14"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        isConflict <- resT[FlowState]((resource.isConflict(_: ReqRespData)).st)
        _ <-
          if (isConflict) resT[FlowState](halt[Boolean](409).point[FlowState])
          else acceptContent(resource)
      } yield p11

      act.run
    }
  }

  /* PUT? */
  lazy val o16: Decision = new Decision {
    def name: String = "v3o16"

    protected def decide(resource: Resource): FlowState[Res[Decision]] =
      testMethod(PUT,result(o14),result(o18))
  }

  /* Multiple Representations?  also do GET/HEAD body rendering here */
  lazy val o18: Decision = new Decision {
    def name: String = "v3o18"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      def ifGetOrHead[A](isTrue: Boolean, f: ReqRespData => (ReqRespData,Res[A]), default: => A) =
        if (isTrue) resT[FlowState](State(f))
        else default.point[ResTFlow]

      def setHeader(header: HTTPHeader, value: Option[String]): ResT[FlowState,Option[String]] =
        ((responseHeadersL member header) := value).liftM[ResT]

      val setBody = for {
      // find content providing function given chosen content type and produce body, setting it in the response
        mbChosenCType <- (metadataL >=> contentTypeL).st.liftM[ResT]
        chosenCType <- resT[FlowState](mbChosenCType.fold(
          some = result(_),
          none = error("internal flow error, missing chosen ctype in o18")
        ).point[FlowState])

        mbProvidedF <- resT[FlowState]((resource.contentTypesProvided(_: ReqRespData)).st) map {
          _.find(_._1 == chosenCType).map(_._2)
        }

        producedBody <- resT[FlowState](((d: ReqRespData) => mbProvidedF.map(_(d)) | ((d, result(Array[Byte]())))).st)
        body <- encodeBody(resource, producedBody).liftM[ResT]
        _ <- (respBodyL := body).liftM[ResT]
      } yield ()

      val act = for {
        doBody <- methodL.map(m => m === GET || m === HEAD).liftM[ResT]
        // set Etag, last mod, and expires if GET or HEAD and they are provided by resource
        mbEtag <- ifGetOrHead(doBody, resource.generateEtag(_), none[String])
        mbLastMod <- ifGetOrHead(doBody, resource.lastModified(_), none[Date])
        mbExpires <- ifGetOrHead(doBody, resource.expires(_), none[Date])
        _ <- setHeader(ETag, mbEtag)
        _ <- setHeader(LastModified, mbLastMod.map(Util.formatDate(_)))
        _ <- setHeader(Expires, mbExpires.map(Util.formatDate(_)))
        _ <- if (doBody) setBody else ().point[ResTFlow]

        // determine if response has multiple choices
        mc <- resT[FlowState]((resource.multipleChoices(_: ReqRespData)).st)
        decision <-
          if (mc) resT[FlowState](halt[Decision](300).point[FlowState])
          else resT[FlowState](halt[Decision](200).point[FlowState])
      } yield decision // we will never actually get to this yield

      act.run
    }
  }

  /* Does Response Have Entity? (response body empty? */
  lazy val o20: Decision = new Decision {
    def name: String = "v3o20"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      body <- respBodyL
    } yield if (body.isEmpty) halt(204) else result(o18)
  }

  /* Is Conflict? (identical impl to o14) */
  lazy val p3: Decision = new Decision {
    def name: String = "v3p3"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val act = for {
        isConflict <- resT[FlowState]((resource.isConflict(_: ReqRespData)).st)
        _ <-
        if (isConflict) resT[FlowState](halt[Boolean](409).point[FlowState])
        else acceptContent(resource)
      } yield p11

      act.run
    }
  }

  /* New Resource? (basically, is location header set?) */
  lazy val p11: Decision = new Decision {
    def name: String = "v3p11"

    protected def decide(resource: Resource): FlowState[Res[Decision]] = for {
      mbLoc <- responseHeadersL member Location
      decision <- mbLoc >| halt[Decision](201).point[FlowState] | result(o20).point[FlowState]
    } yield decision
  }

  /** Helper Functions **/
  private def chooseCharset(resource: Resource, acceptHeader: String): ResT[FlowState, Decision] = {

    val charsetsProvided = resT[FlowState]((resource.charsetsProvided(_: ReqRespData)).st)

    def doChoose(mbProvided: Resource.CharsetsProvided): Res[(Decision, Option[String])] =
      mbProvided.map { provided =>
        Util.chooseCharset(provided.unzip._1, acceptHeader)
          .map(c => result((f6, some(c))))
          .getOrElse(halt(406))
      } getOrElse { result((f6, none)) }

    def setCharsetMeta(chosen: Option[String]): ResT[FlowState,Option[String]] =
      ((metadataL >=> chosenCharsetL) := chosen).liftM[ResT]

    for {
      p <- charsetsProvided
      (decision, chosen) <- resT[FlowState](doChoose(p).point[FlowState])
      _ <- setCharsetMeta(chosen)
    } yield decision
  }

  private def chooseEncoding(resource: Resource, headerValue: String): ResT[FlowState,Decision] = {
    val encodingsProvided = resT[FlowState]((resource.encodingsProvided(_: ReqRespData)).st)

    def doChoose(mbProvided: Resource.EncodingsProvided): Res[(Decision, Option[String])] =
      mbProvided.map { provided =>
        Util.chooseEncoding(provided.unzip._1, headerValue: String)
          .map(e => result((g7, some(e))))
          .getOrElse(halt(406))
      } getOrElse result((g7, none))

    def setEncodingHeader(chosen: Option[String]): FlowState[Option[String]] =
      (responseHeadersL member ContentEncoding) := (chosen filterNot { _ === "identity" })

    for {
      p <- encodingsProvided
      (decision, chosen) <- resT[FlowState](doChoose(p).point[FlowState])
      _ <- ((metadataL >=> chosenEncodingL) := chosen).liftM[ResT]
      _ <- setEncodingHeader(chosen).liftM[ResT]
    } yield decision
  }

  private def headerExists(header: HTTPHeader, exists: Res[Decision], dne: Res[Decision]) = for {
    mbIfMatch <- (requestHeadersL member header)
  } yield mbIfMatch >| exists | dne


  private def testMethod(expected: HTTPMethod,
                         isExpected: Res[Decision],
                         notExpected: Res[Decision]): FlowState[Res[Decision]] = {
    testMethod(expected :: Nil, isExpected, notExpected)
  }


  private def testMethod(expected: List[HTTPMethod],
                         isExpected: Res[Decision],
                         notExpected: Res[Decision]): FlowState[Res[Decision]] = for {
      method <- methodL
    } yield if (expected.contains(method)) isExpected else notExpected


  private def validateDate(headerName: HTTPHeader,
                           valid: Res[Decision],
                           invalid: Res[Decision]): FlowState[Res[Decision]] = for {
      // if we have reached here we have verified the header has value already so we default
      // empty string which should never be reached
      iums <- (requestHeadersL member headerName).map { _ getOrElse "" }
      isValid <- (Util.parseDate(iums) >| true | false).point[FlowState]
    } yield if (isValid) valid else invalid


  private def testDate(resource: Resource,
                       headerName: HTTPHeader,
                       modified: Res[Decision],
                       notModified: Res[Decision])(test: (Long,Long) => Boolean): FlowState[Res[Decision]] = {

    def isModified(mbLastMod: Option[Date], mbHeaderDate: Option[String]) =
      (mbLastMod |@| mbHeaderDate.map(Util.parseDate(_)).getOrElse(none)) {
        (t1,t2) => test(t1.getTime,t2.getTime)
      } getOrElse false

    val act = for {
      mbIums <- (requestHeadersL member headerName).st.liftM[ResT]
      mbLastMod <- resT[FlowState]((resource.lastModified(_: ReqRespData)).st)
      decision <- resT[FlowState] {
        if (isModified(mbLastMod, mbIums)) modified.point[FlowState] else notModified.point[FlowState]
      }
    } yield decision

    act.run
  }


  private def acceptContent(resource: Resource): ResT[FlowState,Boolean] = {
    val reqCType = for {
      contentType <- optionT[FlowState]((requestHeadersL member ContentTypeHeader).st)
      mediaInfo <- optionT[FlowState](Util.acceptToMediaTypes(contentType).headOption.point[FlowState])
    } yield mediaInfo.mediaRange

    for {
      // get request content type
      contentType <- reqCType.getOrElse(ContentType("application/octet-stream")).liftM[ResT]

      // lookup content types accepted and find body prod function
      acceptedList <- resT[FlowState]((resource.contentTypesAccepted(_: ReqRespData)).st)
      mbAcceptableF <- acceptedList.find(_._1 === contentType).map(_._2).point[ResTFlow]

      // if found, run it, call encodeBodyIfSet if it succeeds, 500 otherwise
      // if not found, return halt 415
      didSucceed <- resT[FlowState](((d: ReqRespData) => mbAcceptableF.map(_(d)).getOrElse((d,halt(415)))).st)
      _ <-
        if (didSucceed) encodeBodyIfSet(resource).liftM[ResT]
        else resT[FlowState](halt[HTTPBody](500).point[FlowState]) // TODO: real error message
    } yield didSucceed
  }


  private def encodeBody(resource: Resource, body: HTTPBody): FlowState[HTTPBody] = for {
    mbCharset <- metadataL >=> chosenCharsetL
    mbProvidedCh <- (resource.charsetsProvided(_: ReqRespData)).st.map(_.getOrElse(None))
    mbEncoding <- metadataL >=> chosenEncodingL
    mbProvidedEnc <- (resource.encodingsProvided(_: ReqRespData)).st.map(_.getOrElse(None))
    charsetter <- (((mbProvidedCh |@| mbCharset) {
      (p,c)  => p.find(_._1 === c)
    }).join.fold(some = _._2, none = identity[Array[Byte]](_))).point[FlowState]
    encoder <- (((mbProvidedEnc |@| mbEncoding) {
      (p,e) => p.find(_._1 === e)
    }).join.fold(some = _._2, none = identity[Array[Byte]](_))).point[FlowState]
  } yield encoder(charsetter(body.bytes)) // TODO: handle streamed bodies


  private def encodeBodyIfSet(resource: Resource): FlowState[Unit] = for {
    body <- respBodyL
    newBody <- if (!body.isEmpty) encodeBody(resource, body) else body.point[FlowState]
    _ <- respBodyL := newBody
  } yield ()
}
