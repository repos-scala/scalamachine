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

sealed trait Result[+T] {
  def context: Context
  def data: ReqRespData
    
  private[core] def setData(data: ReqRespData): Result[T]
}

case class SimpleResult[T](value: T, data: ReqRespData, context: Context) extends Result[T] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}
case class ErrorResult(error: Any, data: ReqRespData, context: Context) extends Result[Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}
case class HaltResult(code: Int, data: ReqRespData, context: Context) extends Result[Nothing] {
  private[core] def setData(d: ReqRespData) = copy(data = d)
}

sealed trait AuthResult
case object AuthSuccess extends AuthResult
case class AuthFailure(headerValue: String) extends AuthResult


trait Context
trait Resource {
  def serviceAvailable(data: ReqRespData, ctx: Context): Result[Boolean]
  def knownMethods(data: ReqRespData, ctx: Context): Result[List[HTTPMethod]]
  def uriTooLong(data: ReqRespData, ctx: Context): Result[Boolean]
  def allowedMethods(data: ReqRespData, ctx: Context): Result[List[HTTPMethod]]
  def isMalformed(data: ReqRespData, ctx: Context): Result[Boolean]
  def isAuthorized(data: ReqRespData, ctx: Context): Result[AuthResult]
}

trait Decision {

  
  def name: String

  // we use any for the type parameterizing Result because the value is not required at higher levels
  def decide(resource: Resource, data: ReqRespData, ctx: Context): (Result[Any], Option[Decision])

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

}

object Decision {

  private[this] def setStatus[T](code: Int): SimpleResult[T] => ReqRespData = _.data.setStatusCode(code)

  def apply[T](decisionName: String, 
               expected: T, 
               test: Resource => (ReqRespData, Context) => Result[T],
               onSuccess: Decision,
               onFailure: Int): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(setStatus[T](onFailure)))
  
  def apply[T](decisionName: String, 
               expected: T, 
               test: Resource => (ReqRespData, Context) => Result[T],
               onSuccess: Int, 
               onFailure: Decision): Decision = apply(decisionName, expected, test, Left(setStatus[T](onSuccess)), Right(onFailure))
  
  def apply[T](decisionName: String,
               expected: T, 
               test: Resource => (ReqRespData,Context) => Result[T],
               onSuccess: Decision,
               onFailure: SimpleResult[T] => ReqRespData): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(onFailure))
  
  def apply[T](decisionName: String,
                   expected: T,
                   test: Resource => (ReqRespData,Context) => Result[T],
                   onSuccess: Either[SimpleResult[T] => ReqRespData, Decision],
                   onFailure: Either[SimpleResult[T] => ReqRespData, Decision]): Decision = apply(decisionName, test, (res: T, _: ReqRespData) => res == expected, onSuccess, onFailure)

  
  def apply[T](decisionName: String, test: Resource => (ReqRespData,Context) => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Decision, onFailure: Int): Decision = apply(decisionName, test, check, onSuccess, setStatus[T](onFailure))
  
  def apply[T](decisionName: String, test: Resource => (ReqRespData,Context) => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Decision, onFailure: SimpleResult[T] => ReqRespData): Decision = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))
  
  def apply[T](decisionName: String, test: Resource => (ReqRespData,Context) => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Either[SimpleResult[T] => ReqRespData, Decision], onFailure: Either[SimpleResult[T] => ReqRespData, Decision]): Decision = new Decision {
    
    def name = decisionName
    
    def decide(resource: Resource, data: ReqRespData, context: Context): (Result[T], Option[Decision]) = {
      test(resource)(data, context) match {
        case result @ SimpleResult(value, newData, newContext) =>
          if (check(value, newData)) handle(result, onSuccess)
          else handle(result, onFailure)
        case result => (result, None)
      }
    }

    def handle(result: SimpleResult[T], handle: Either[SimpleResult[T] => ReqRespData, Decision]) = handle match {
      case Left(f) => (result.setData(f(result)), None)
      case Right(d) => (result, Some(d))
    }
    
  }
  
}

// methods are split up to allow stacking at start/end of flow as well as start/end of each decision
// run decision is pretty much extraneous outside of that
// TODO: rethink how stacking is allowed here, intention is to allow stacking at start/end flow and start/end decision
trait FlowRunnerBase {    

  def run(decision: Decision, resource: Resource, data: ReqRespData, ctx: Context): ReqRespData

  // Result[Any] is used here because the decision result type is not actually cared about at this layer
  protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData, ctx: Context): (Result[Any], Option[Decision])

}

trait FlowRunner extends FlowRunnerBase {

  // can't mark this tail recursive if we want it to be stackable :(
  // can build an "optimized trait" later
  def run(decision: Decision, resource: Resource, data: ReqRespData, ctx: Context): ReqRespData = {
    runDecision(decision, resource, data, ctx) match {
      case (SimpleResult(_, newData, newContext), Some(nextDecision)) => run(nextDecision, resource, newData, newContext)
      case (ErrorResult(_, newData, _), _) => newData.setStatusCode(500)
      case (HaltResult(code, newData, _), _) => newData.setStatusCode(code)
      case (result, _) => result.data
    }
  }

  protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData, ctx: Context): (Result[Any], Option[Decision]) =
    decision.decide(resource, data, ctx)

}