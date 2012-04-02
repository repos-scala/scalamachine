package com.github.jrwest.scalamachine.core

trait Resource {
  import Resource.ContentTypesProvided

  //def init: C
  def serviceAvailable(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(true), data)

  def knownMethods(data: ReqRespData): (Res[List[HTTPMethod]],ReqRespData)= (
    default(List(OPTIONS, TRACE, CONNECT, HEAD, GET, POST, PUT, DELETE)),
    data
  )

  def uriTooLong(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(false), data)

  def allowedMethods(data: ReqRespData): (Res[List[HTTPMethod]],ReqRespData) = (default(GET :: Nil), data)

  def isMalformed(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(false), data)

  def isAuthorized(data: ReqRespData): (Res[AuthResult],ReqRespData) = (default(AuthSuccess), data)

  def isForbidden(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(false), data)

  def contentHeadersValid(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(true), data)

  def isKnownContentType(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(true), data)

  def isValidEntityLength(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(true), data)

  def options(data: ReqRespData): (Res[Map[String, String]],ReqRespData) = (default(Map()), data)

  def contentTypesProvided(data: ReqRespData): (Res[ContentTypesProvided],ReqRespData) = {
    (default((ContentType("text/html") -> defaultResponse) :: Nil), data)
  }

  // TODO: change to real content negotiation like ruby port
  def isLanguageAvailable(data: ReqRespData): (Res[Boolean],ReqRespData) = (default(true), data)


  private def default[A](value: A): Res[A] = ValueRes(value)

  private val defaultResponse: ReqRespData => (Res[String], ReqRespData) = (default("hello, scalamachine"), _)

}

object Resource {
  type ContentTypesProvided = List[(ContentType, ReqRespData => (Res[String],ReqRespData))]
}

