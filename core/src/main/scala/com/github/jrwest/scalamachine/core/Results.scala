package com.github.jrwest.scalamachine.core

sealed trait Result[+T] {
  def data: ReqRespData

  private[core] def setData(data: ReqRespData): Result[T]
}

case class SimpleResult[T](value: T, data: ReqRespData) extends Result[T] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}
case class ErrorResult(error: Any, data: ReqRespData) extends Result[Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}
case class HaltResult(code: Int, data: ReqRespData) extends Result[Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}

case class EmptyResult(data: ReqRespData) extends Result[Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}

object HaltResult {
  def apply(code: Int, headers: Map[String,String], data: ReqRespData): HaltResult = HaltResult(code, data.mergeResponseHeaders(headers))
}

sealed trait AuthResult
case object AuthSuccess extends AuthResult
case class AuthFailure(headerValue: String) extends AuthResult
