package com.github.jrwest.scalamachine.core

trait Resource {
  //def init: C
  def serviceAvailable(data: ReqRespData): Result[Boolean] = default(true, data)

  def knownMethods(data: ReqRespData): Result[List[HTTPMethod]] = default(
    List(OPTIONS, TRACE, CONNECT, HEAD, GET, POST, PUT, DELETE),
    data
  )

  def uriTooLong(data: ReqRespData): Result[Boolean] = default(false, data)

  def allowedMethods(data: ReqRespData): Result[List[HTTPMethod]] = default(GET :: Nil, data)

  def isMalformed(data: ReqRespData): Result[Boolean] = default(false, data)

  def isAuthorized(data: ReqRespData): Result[AuthResult] = default(AuthSuccess, data)

  def isForbidden(data: ReqRespData): Result[Boolean] = default(false, data)

  def contentHeadersValid(data: ReqRespData): Result[Boolean] = default(true, data)

  def isKnownContentType(data: ReqRespData): Result[Boolean] = default(true, data)

  def isValidEntityLength(data: ReqRespData): Result[Boolean] = default(true, data)

  def options(data: ReqRespData): Result[Map[String, String]] = default(Map(), data)

  def contentTypesProvided(data: ReqRespData): Result[List[(ContentType, ReqRespData => Result[String])]] = {
    default((ContentType("text/html") -> defaultResponse) :: Nil, data)
  }


  private def default[A](value: A, data: ReqRespData): Result[A] = SimpleResult(value, data)

  private val defaultResponse: ReqRespData => Result[String] = default("hello, scalamachine", _)

}

