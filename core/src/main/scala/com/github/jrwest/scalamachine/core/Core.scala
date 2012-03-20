package com.github.jrwest.scalamachine.core

// TODO: split all of the below into seperate files/packages etc (cleanup)

case class ReqRespData(
                        pathParts: List[String] = Nil,
                        method: HTTPMethod = GET,
                        statusCode: Int = 200,
                        responseHeaders: Map[String,String] = Map()
                      ) {

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
  override def toString = "HEAD"
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

object HTTPMethod {
  // currently, unknown strings are assumed to be GETS
  // this is under assumption that wherever scalamachine is embedded
  // handles unknown HTTP methods properly, thinking about changing to an unapply
  // and letting implementations decide on how to handle the option (throw/fold/fail/etc)
  def fromString(methodStr: String): HTTPMethod = methodStr.toLowerCase match {
    case "OPTIONS" => OPTIONS
    case "HEAD" => HEAD
    case "TRACE" => TRACE
    case "CONNECT" => CONNECT
    case "DELETE" => DELETE
    case "PUT" => PUT
    case "POST" => POST
    case _ => GET
  }
}

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


trait Resource {    
  //def init: C
  def serviceAvailable(data: ReqRespData): Result[Boolean] = default(true, data)
  def knownMethods(data: ReqRespData): Result[List[HTTPMethod]] = default(
    List(OPTIONS,TRACE,CONNECT,HEAD,GET,POST,PUT,DELETE), 
    data
  )
  def uriTooLong(data: ReqRespData): Result[Boolean] = default(false,data)
  def allowedMethods(data: ReqRespData): Result[List[HTTPMethod]] = default(GET :: Nil, data)
  def isMalformed(data: ReqRespData): Result[Boolean] = default(false,data)
  def isAuthorized(data: ReqRespData): Result[AuthResult] = default(AuthSuccess,data)
  def isForbidden(data: ReqRespData): Result[Boolean] = default(false,data)
  def contentHeadersValid(data: ReqRespData): Result[Boolean] = default(true,data)
  def isKnownContentType(data: ReqRespData): Result[Boolean] = default(true,data)
  def isValidEntityLength(data: ReqRespData): Result[Boolean] = default(true,data)
  def options(data: ReqRespData): Result[Map[String, String]] = default(Map(), data)
  
  private def default[A](value: A, data: ReqRespData): Result[A] = SimpleResult(value,data) 
}

trait Decision {

  
  def name: String

  // we use any for the type parameterizing Result because the value is not required at higher levels
  def decide(resource: Resource, data: ReqRespData): (Result[Any], Option[Decision])

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

}

object Decision {

  private[this] def setStatus[T](code: Int): SimpleResult[T] => ReqRespData = _.data.setStatusCode(code)

  def apply[T](decisionName: String,
                 expected: T,
                 test: Resource => ReqRespData => Result[T],
                 onSuccess: Decision,
                 onFailure: Int): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(setStatus[T](onFailure)))
  
  def apply[T](decisionName: String,
                 expected: T,
                 test: Resource => ReqRespData => Result[T],
                 onSuccess: Int,
                 onFailure: Decision): Decision = apply(decisionName, expected, test, Left(setStatus[T](onSuccess)), Right(onFailure))
  
  def apply[T](decisionName: String,
                 expected: T,
                 test: Resource => ReqRespData => Result[T],
                 onSuccess: Decision,
                 onFailure: SimpleResult[T] => ReqRespData): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(onFailure))
  
  def apply[T](decisionName: String,
                 expected: T,
                 test: Resource => ReqRespData => Result[T],
                 onSuccess: Either[SimpleResult[T] => ReqRespData, Decision],
                 onFailure: Either[SimpleResult[T] => ReqRespData, Decision]): Decision = apply(decisionName, test, (res: T, _: ReqRespData) => res == expected, onSuccess, onFailure)

  
  def apply[T](decisionName: String, test: Resource => ReqRespData => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Decision, onFailure: Int): Decision = apply(decisionName, test, check, onSuccess, setStatus[T](onFailure))
  
  def apply[T](decisionName: String, test: Resource => ReqRespData => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Decision, onFailure: SimpleResult[T] => ReqRespData): Decision = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))
  
  def apply[T](decisionName: String, test: Resource => ReqRespData => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Either[SimpleResult[T] => ReqRespData, Decision], onFailure: Either[SimpleResult[T] => ReqRespData, Decision]): Decision = new Decision {
    
    def name = decisionName
    
    def decide(resource: Resource, data: ReqRespData): (Result[T], Option[Decision]) = {
      test(resource)(data) match {
        case result @ SimpleResult(value, newData) =>
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
trait FlowRunnerBase {

  // stacking this method gives access to start/end of flow
  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData

  // stack this method to access start/end descision
  protected def runDecisionOuter(resource: Resource, decision: Decision, data: ReqRespData): ReqRespData

  // same as above but with access to decision result and next decision info
  protected def runDecisionInner(resource: Resource, decision: Decision, data: ReqRespData): (Result[Any],Option[Decision])

}

class FlowRunner extends FlowRunnerBase {

  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData = {
    runDecisionOuter(resource,decision,data)
  }
  
  // can't mark this tail recursive if we want it to be stackable :(
  // can build an "optimized trait" later
  protected def runDecisionOuter(resource: Resource, decision: Decision, data: ReqRespData): ReqRespData = {
    runDecisionInner(resource,decision,data) match {
      case (SimpleResult(_, newData), Some(nextDecision)) => runDecisionOuter(resource, nextDecision, newData)
      case (ErrorResult(_, newData), _) => newData.setStatusCode(500)
      case (HaltResult(code, newData), _) => newData.setStatusCode(code)
      case (result, _) => result.data
    }
  }

  protected def runDecisionInner(resource: Resource, decision: Decision, data: ReqRespData) = decision.decide(resource, data)
}