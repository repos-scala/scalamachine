package com.github.jrwest.scalamachine.core

trait WebmachineDecisions {

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
      (r: SimpleResult[List[HTTPMethod]]) => r.data.setStatusCode(405).setResponseHeader("Allow", r.value.map(_.toString).mkString(", "))
    )

  /* Malformed Request? */
  lazy val b9: Decision = Decision("v3b9", true, (r: Resource) => r.isMalformed(_: ReqRespData), 400, b8)

  /* Is Authorized? */
  lazy val b8: Decision = Decision(
    "v3b8",
    AuthSuccess,
    (r: Resource) => r.isAuthorized(_: ReqRespData),
    b7,
    (r: SimpleResult[AuthResult]) => r.value match {
      case AuthFailure(headerVal) => r.data.setResponseHeader("WWW-Authenticate", headerVal).setStatusCode(401);
      case _ => r.data
    }
  )

  /* Is Forbidden? */
  lazy val b7: Decision = Decision("v3b7",true,(r: Resource) => r.isForbidden(_: ReqRespData), 403, b6)

  /* Content-* Headers Are Valid? */
  lazy val b6: Decision = Decision("v3b6",true,(r: Resource) => r.contentHeadersValid(_: ReqRespData), b5, 501)

  /* Is Known Content-Type? */
  lazy val b5: Decision = Decision("v3b5",true,(r: Resource) => r.isKnownContentType(_: ReqRespData), b4, 415)

  /* Request Entity Too Large? */
  lazy val b4: Decision = Decision("v3b4",true,(r: Resource) => r.isValidEntityLength(_: ReqRespData), b3, 413)

  /* OPTIONS? */
  lazy val b3: Decision = new Decision {
    val name: String = "v3b3"

    def decide(resource: Resource, data: ReqRespData): (Result[Any], Option[Decision]) = {
      data.method match {
        case OPTIONS => {
          val hdrs = resource.options(data) match { case SimpleResult(hs,_) => hs; case _ => Map[String,String]() }
          (HaltResult(200, hdrs, data), None)
        }
        case _ => (EmptyResult(data), Some(c3))
      }
    }
  }

  /* Accept Exists? */
  lazy val c3: Decision = new Decision {
    val name: String = "v3c3"
    
    def decide(resource: Resource,  data: ReqRespData): (Result[Any],Option[Decision]) = {
      data.requestHeader("Accept") match {
        case Some(_) => (EmptyResult(data),Some(c4))
        case None => {
          // TODO: change this to handle empty list of content types
          // TODO: get rid of hacky cast
          val cType = resource.contentTypesProvided(data).asInstanceOf[SimpleResult[List[(ContentType,ReqRespData => Result[String])]]].value.head._1
          val newData = data.copy(metadata = data.metadata.copy(contentType = Option(cType)))
          (EmptyResult(newData),Some(d4))
        }
      }
    }
  }

  /* Acceptable Media Type Available? */
  lazy val c4: Decision = null
  
  lazy val d4: Decision = null
}