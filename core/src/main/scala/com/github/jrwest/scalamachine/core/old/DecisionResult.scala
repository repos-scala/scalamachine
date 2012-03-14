/*package com.github.jrwest.scalamachine.core


trait DecisionResult[+T] {
  def value: T
  def data: ReqRespData
  def context: Context
}
case class Result[T](value: T, data: ReqRespData, context: Context) extends DecisionResult[T]
case class HaltResult(code: Int, data: ReqRespData, context: Context) extends DecisionResult[Int] {
  val value = code
}
// TODO probably something better than Any here
case class ErrorResult(reason: Any,  data: ReqRespData, context: Context) extends DecisionResult[Any] {
  val value = reason
}*/



