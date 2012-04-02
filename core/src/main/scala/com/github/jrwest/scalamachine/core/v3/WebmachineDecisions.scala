package com.github.jrwest.scalamachine.core
package v3

import flow._
import scalaz.{State,StateT}
import scalaz.syntax.functor._


trait WebmachineDecisions {
  
  import ReqRespData._
  import Metadata._
  import Resource.ContentTypesProvided

  /* Service Available? */
  lazy val b13: Decision = Decision("v3b13", true, (r: Resource) => r.serviceAvailable(_: ReqRespData), b12, 503)

  /* Known Methods */
  lazy val b12: Decision = Decision("v3b12", (r: Resource) => r.knownMethods(_: ReqRespData), (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method), b11, 501)

  /* URI Too Long? */
  lazy val b11: Decision = Decision("v3b11", true, (r: Resource) => r.uriTooLong(_: ReqRespData), 414, b10)

  /* Allowed Methods */
  lazy val b10: Decision =
    Decision(
      "v3b10",
      (r: Resource) => r.allowedMethods(_: ReqRespData),
      (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method),
      b9,
      (r: ValueRes[List[HTTPMethod]], data: ReqRespData) => {
        val notAllowed: StateT[ValueRes,ReqRespData,Unit] = for {
          v <- StateT.StateMonadTrans.liftM(r)
          _ <- (statusCodeL := 405).lift[ValueRes]
          _ <- (responseHeadersL += (("Allow" -> v.map(_.toString).mkString(", ")))).lift[ValueRes]
        } yield ()
        notAllowed.exec(data).value
      }
    )

  /* Malformed Request? */
  lazy val b9: Decision = Decision("v3b9", true, (r: Resource) => r.isMalformed(_: ReqRespData), 400, b8)

  /* Is Authorized? */
  lazy val b8: Decision = Decision(
    "v3b8",
    AuthSuccess,
    (r: Resource) => r.isAuthorized(_: ReqRespData),
    b7,
    (r: ValueRes[AuthResult], data: ReqRespData) => (for {
      (authR: AuthResult) <- StateT.StateMonadTrans.liftM(r)
      _ <- authR.fold(
        failure = (failMsg: String) => (responseHeadersL += ("WWW-Authenticate" -> failMsg)).lift[ValueRes],
        success = responseHeadersL.lift[ValueRes]
      )
      _ <- (statusCodeL := 401).lift[ValueRes]
    } yield ()).exec(data).value
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
    def decide(resource: Resource, data: ReqRespData): (Res[Any], ReqRespData, Option[Decision]) = {
      data.method match {
        case OPTIONS => {
          val (res, newData) = resource.options(data)
          val setHeaders: StateT[Res, ReqRespData,Unit] = for {
            hdrs <- StateT.StateMonadTrans.liftM(res)
            _ <- (responseHeadersL ++= hdrs.toList).lift[Res]
          } yield ()
          (HaltRes(200), (setHeaders exec newData) | newData, None)
        }
        case _ => (EmptyRes, data, Some(c3))
      }
    }
  }

  /* Accept Exists? */
  lazy val c3: Decision = new Decision {
    import scalaz.syntax.std.allV.ToListVFromList

    // TODO: move somewhere more global it will probably be used elsewhere
    val defaultContentType = ContentType("text/plain")

    val name: String = "v3c3"

    def decide(resource: Resource, data: ReqRespData): (Res[Any], ReqRespData, Option[Decision]) = {
      val (nextDecision, finalData) = performDecision(resource)(data)

      (EmptyRes,finalData,Some(nextDecision))
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
  lazy val c4: Decision = null

  lazy val d4: Decision = null
}