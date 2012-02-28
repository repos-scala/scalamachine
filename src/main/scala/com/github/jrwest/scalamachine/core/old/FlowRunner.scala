/*package com.github.jrwest.scalamachine.core


trait Context
trait ReqRespData

trait ResourceCallResult {
  def value
  def data
  def context
} 

// either ReqRespData, ResourceCallResult, Decision, Http Code
trait DecisionResult[T] {
  def value: T
}


trait Resource {
  
  def serviceAvailable[C <: Context](data: ReqRespData, ctx: Context)
  
}

trait Decision {
  def decide(resource: Resource, data: ReqRespData, ctx: Context): Either[ReqRespData, Decision]
}

object Decision {
  def decideOnExpectedValue[T](exp: T, f: Resource => ResourceCallResult)
}

trait FlowRunnerBase {

  // type of the starting decision
  type SD

  protected def startDecision: Decision[SD]
  
  def run[C <: Context](resource: Resource, data: ReqRespData, ctx: C): ReqRespData
  
  protected def runDecision[T,C <: Context,A](resource: Resource, data: ReqRespData, ctx: C, decision: Decision[T]): (DecisionResult[T], Either[Int,Decision[A]])
  
}

trait FlowRunner extends FlowRunnerBase {
  
  def run[C <: Context](resource: Resource, data: ReqRespData, ctx: C): ReqRespData = {
    null
  }
  
  protected def runDecision[T,C <: Context,A](resource: Resource, data: ReqRespData, ctx: C, decision: Decision[T]): (DecisionResult[T], Either[Int,Decision[A]]) = {
    val result = decision.call(resource, data, ctx)    
    val nextDecision: Either[Int,Decision[A]] = if (result.value == decision.expected) decision.trueBranch else decision.falseBranch
    (result, nextDecision)
  }
    
}*/