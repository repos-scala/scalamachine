/*package com.github.jrwest.scalamachine.core

sealed trait Decision[+T] {
  type TA
  type TB
  //type R <: Context
  def expected: T
  def call(resource: Resource, data: ReqRespData, ctx: Context): DecisionResult[T]
  def trueBranch: Either[Int,Decision[TA]]
  def falseBranch: Either[Int,Decision[TB]]
}

object Decision {
  def apply[T,A,B](exp: T,
                 fn: Resource => (ReqRespData, Context) => DecisionResult[T],
                 whenTrue: Either[Int, Decision[A]],
                 whenFalse: Either[Int,Decision[B]]) = new Decision[T] {
    type TA = A
    type TB = B
    //type R = C
    def expected = exp
    def call(resource: Resource, data: ReqRespData, ctx: Context) = fn(resource)(data,ctx)
    def trueBranch = whenTrue
    def falseBranch = whenFalse
  }
} */