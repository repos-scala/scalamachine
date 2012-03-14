package com.github.jrwest.scalamachine.core

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 2/27/12
 * Time: 11:15 PM 
 */

// TODO: split all of the below into seperate files/packages etc (cleanup)

trait ReqRespData {
  
  /* Request Data */
  def method: HTTPMethod
  
  /* Response Data */
  def statusCode: Int
  def setStatusCode(code: Int): ReqRespData

  def responseHeaders: Map[String, String]
  def responseHeader(name: String): Option[String]
  def setResponseHeader(name: String, value: String): ReqRespData
}

// TODO: this was kind of whipped together. doesn't make sense to initialize some of the response data
// may need something similar to scaliak's options or shadow fields that are options, or an apply helper
// also not sure we need both the trait and the case class, just case class may suffice
case class ImmutableReqRespData(method: HTTPMethod, statusCode: Int = 200, responseHeaders: Map[String,String] = Map()) extends ReqRespData {
  def setStatusCode(code: Int) = copy(statusCode = code)
  def responseHeader(name: String) = responseHeaders.get(name.toLowerCase)
  def setResponseHeader(name: String, value: String) = copy(responseHeaders = responseHeaders + (name.toLowerCase -> value))
}

// not so sure about these yet, was done in a hurry
trait HTTPMethod
case object GET extends HTTPMethod {
  override def toString = "GET"
}
case object HEAD extends HTTPMethod {
  override def toString = "POST"
}
case object POST extends HTTPMethod {
  override def toString = "POST"
}
case object PUT extends HTTPMethod {
  override def toString = "PUT"
}
case object DELETE extends HTTPMethod {
  override def toString = "DELETE"
}
case object TRACE extends HTTPMethod {
  override def toString = "TRACE"
}
case object CONNECT extends HTTPMethod {
  override def toString = "CONNECT"
}
case object OPTIONS extends HTTPMethod {
  override def toString = "OPTIONS"
}

sealed trait Result[C,+T] {
  def context: C
  def data: ReqRespData
    
  private[core] def setData(data: ReqRespData): Result[C,T]
}

case class SimpleResult[C,T](value: T, data: ReqRespData, context: C) extends Result[C,T] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}
case class ErrorResult[C](error: Any, data: ReqRespData, context: C) extends Result[C,Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}
case class HaltResult[C](code: Int, data: ReqRespData, context: C) extends Result[C,Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}

sealed trait AuthResult
case object AuthSuccess extends AuthResult
case class AuthFailure(headerValue: String) extends AuthResult


trait Resource[C] {
  def init: C  
  def serviceAvailable(data: ReqRespData, ctx: C): Result[C,Boolean]
  def knownMethods(data: ReqRespData, ctx: C): Result[C,List[HTTPMethod]]
  def uriTooLong(data: ReqRespData, ctx: C): Result[C,Boolean]
  def allowedMethods(data: ReqRespData, ctx: C): Result[C,List[HTTPMethod]]
  def isMalformed(data: ReqRespData, ctx: C): Result[C,Boolean]
  def isAuthorized(data: ReqRespData, ctx: C): Result[C,AuthResult]
  def isForbidden(data: ReqRespData, ctx: C): Result[C,Boolean]
}

trait Decision[C] {

  
  def name: String

  // we use any for the type parameterizing Result because the value is not required at higher levels
  def decide(resource: Resource[C], data: ReqRespData, ctx: C): (Result[C,Any], Option[Decision[C]])

  override def equals(o: Any): Boolean = o match {
    case o: Decision[_] => o.name == name
    case _ => false
  }

}

object Decision {

  private[this] def setStatus[T,C](code: Int): SimpleResult[C,T] => ReqRespData = _.data.setStatusCode(code)

  def apply[T,C](decisionName: String,
                 expected: T,
                 test: Resource[C] => (ReqRespData, C) => Result[C,T],
                 onSuccess: Decision[C],
                 onFailure: Int): Decision[C] = apply(decisionName, expected, test, Right(onSuccess), Left(setStatus[T,C](onFailure)))
  
  def apply[T,C](decisionName: String,
                 expected: T,
                 test: Resource[C] => (ReqRespData, C) => Result[C,T],
                 onSuccess: Int,
                 onFailure: Decision[C]): Decision[C] = apply(decisionName, expected, test, Left(setStatus[T,C](onSuccess)), Right(onFailure))
  
  def apply[T,C](decisionName: String,
                 expected: T,
                 test: Resource[C] => (ReqRespData,C) => Result[C,T],
                 onSuccess: Decision[C],
                 onFailure: SimpleResult[C,T] => ReqRespData): Decision[C] = apply(decisionName, expected, test, Right(onSuccess), Left(onFailure))
  
  def apply[T,C](decisionName: String,
                 expected: T,
                 test: Resource[C] => (ReqRespData,C) => Result[C,T],
                 onSuccess: Either[SimpleResult[C,T] => ReqRespData, Decision[C]],
                 onFailure: Either[SimpleResult[C,T] => ReqRespData, Decision[C]]): Decision[C] = apply(decisionName, test, (res: T, _: ReqRespData) => res == expected, onSuccess, onFailure)

  
  def apply[T,C](decisionName: String, test: Resource[C] => (ReqRespData,C) => Result[C,T], check: (T, ReqRespData) => Boolean, onSuccess: Decision[C], onFailure: Int): Decision[C] = apply(decisionName, test, check, onSuccess, setStatus[T,C](onFailure))
  
  def apply[T,C](decisionName: String, test: Resource[C] => (ReqRespData,C) => Result[C,T], check: (T, ReqRespData) => Boolean, onSuccess: Decision[C], onFailure: SimpleResult[C,T] => ReqRespData): Decision[C] = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))
  
  def apply[T,C](decisionName: String, test: Resource[C] => (ReqRespData,C) => Result[C,T], check: (T, ReqRespData) => Boolean, onSuccess: Either[SimpleResult[C,T] => ReqRespData, Decision[C]], onFailure: Either[SimpleResult[C,T] => ReqRespData, Decision[C]]): Decision[C] = new Decision[C] {
    
    def name = decisionName
    
    def decide(resource: Resource[C], data: ReqRespData, context: C): (Result[C,T], Option[Decision[C]]) = {
      test(resource)(data, context) match {
        case result @ SimpleResult(value, newData, newContext) =>
          if (check(value, newData)) handle(result, onSuccess)
          else handle(result, onFailure)
        case result => (result, None)
      }
    }

    def handle(result: SimpleResult[C,T], handle: Either[SimpleResult[C,T] => ReqRespData, Decision[C]]) = handle match {
      case Left(f) => (result.setData(f(result)), None)
      case Right(d) => (result, Some(d))
    }
    
  }
  
}

// methods are split up to allow stacking at start/end of flow as well as start/end of each decision
// run decision is pretty much extraneous outside of that
// TODO: rethink how stacking is allowed here, intention is to allow stacking at start/end flow and start/end decision
trait FlowRunnerBase[C] {

  // stacking this method gives access to start/end of flow
  def run(decision: Decision[C], resource: Resource[C], data: ReqRespData): ReqRespData

  // stack this method to access start/end descision
  protected def runDecisionOuter(resource: Resource[C], decision: Decision[C], data: ReqRespData, ctx: C): ReqRespData

  // same as above but with access to decision result and next decision info
  protected def runDecisionInner(resource: Resource[C], decision: Decision[C], data: ReqRespData, ctx: C): (Result[C,Any],Option[Decision[C]])    

}

trait FlowRunner[C] extends FlowRunnerBase[C] {

  def run(decision: Decision[C], resource: Resource[C], data: ReqRespData): ReqRespData = {
    runDecisionOuter(resource,decision,data,resource.init)    
  }
  
  // can't mark this tail recursive if we want it to be stackable :(
  // can build an "optimized trait" later
  protected def runDecisionOuter(resource: Resource[C], decision: Decision[C], data: ReqRespData, ctx: C): ReqRespData = {
    runDecisionInner(resource,decision,data,ctx) match {
      case (SimpleResult(_, newData, newContext), Some(nextDecision)) => runDecisionOuter(resource, nextDecision, newData, newContext)
      case (ErrorResult(_, newData, _), _) => newData.setStatusCode(500)
      case (HaltResult(code, newData, _), _) => newData.setStatusCode(code)
      case (result, _) => result.data
    }
  }

  protected def runDecisionInner(resource: Resource[C], decision: Decision[C], data: ReqRespData, ctx: C) = decision.decide(resource, data, ctx)
}