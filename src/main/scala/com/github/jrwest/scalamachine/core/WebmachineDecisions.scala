package com.github.jrwest.scalamachine.core

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 3/11/12
 * Time: 11:35 PM
 */

trait WebmachineDecisions {
  lazy val b13: Decision = Decision("v3b13", true, (r: Resource) => r.serviceAvailable(_: ReqRespData), b12, 503)
  lazy val b12: Decision = Decision("v3b12", (r: Resource) => r.knownMethods(_: ReqRespData), (l: List[HTTPMethod], d: ReqRespData) => l.contains(d.method), b11, 501)
  lazy val b11: Decision = Decision("v3b11", true, (r: Resource) => r.uriTooLong(_: ReqRespData), 414, b10)
  lazy val b10: Decision = null
}