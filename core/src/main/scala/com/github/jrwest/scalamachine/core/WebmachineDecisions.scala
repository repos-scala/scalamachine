package com.github.jrwest.scalamachine.core

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 3/11/12
 * Time: 11:35 PM
 */

trait WebmachineDecisions {

  /* Service Available? */
  def b13[C]: Decision[C] = Decision("v3b13", true, (r: Resource[C]) => r.serviceAvailable(_: ReqRespData, _: C), b12[C], 503)

  /* Known Methods */
  def b12[C]: Decision[C] = Decision("v3b12", (r: Resource[C]) => r.knownMethods(_: ReqRespData, _: C), (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method), b11[C], 501)

  /* URI Too Long? */
  def b11[C]: Decision[C] = Decision("v3b11", true, (r: Resource[C]) => r.uriTooLong(_: ReqRespData, _: C), 414, b10[C])

  /* Allowed Methods */
  def b10[C]: Decision[C] =
    Decision(
      "v3b10",
      (r: Resource[C]) => r.allowedMethods(_: ReqRespData, _: C),
      (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method),
      b9[C],
      (r: SimpleResult[C,List[HTTPMethod]]) => r.data.setStatusCode(405).setResponseHeader("Allow", r.value.map(_.toString).mkString(", "))
    )

  /* Malformed Request? */
  def b9[C]: Decision[C] = Decision("v3b9", true, (r: Resource[C]) => r.isMalformed(_: ReqRespData, _: C), 400, b8[C])

  /* Is Authorized? */
  def b8[C]: Decision[C] = Decision(
    "v3b8",
    AuthSuccess,
    (r: Resource[C]) => r.isAuthorized(_: ReqRespData, _: C),
    b7[C],
    (r: SimpleResult[C,AuthResult]) => r.value match {
      case AuthFailure(headerVal) => r.data.setResponseHeader("WWW-Authenticate", headerVal).setStatusCode(401);
      case _ => r.data
    }
  )
  
  def b7[C]: Decision[C] = Decision("v3b7",true,(r: Resource[C]) => r.isForbidden(_: ReqRespData, _: C), 403, b6[C])
  
  def b6[C]: Decision[C] = null
}