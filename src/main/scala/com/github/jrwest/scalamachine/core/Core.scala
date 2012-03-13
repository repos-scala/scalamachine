package com.github.jrwest.scalamachine.core

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 2/27/12
 * Time: 11:15 PM 
 */

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

trait Result[+T] {
  def value: T
  def data: ReqRespData
}

case class SimpleResult[T](value: T, data: ReqRespData) extends Result[T]

trait Resource {
  def serviceAvailable(data: ReqRespData): Result[Boolean]
  def knownMethods(data: ReqRespData): Result[List[HTTPMethod]]
  def uriTooLong(data: ReqRespData): Result[Boolean]
  def allowedMethods(data: ReqRespData): Result[List[HTTPMethod]]
  def isMalformed(data: ReqRespData): Result[Boolean]
  def isAuthorized(data: ReqRespData): Result[Boolean]
}

trait Decision {

  def name: String

  def decide(resource: Resource, data: ReqRespData): (ReqRespData, Option[Decision])

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

}

object Decision {

  private[this] def setStatus[T](code: Int): Result[T] => ReqRespData = _.data.setStatusCode(code)

  def apply[T](decisionName: String, 
               expected: T, 
               test: Resource => ReqRespData => Result[T],
               onSuccess: Decision,
               onFailure: Int): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(setStatus(onFailure)))
  
  def apply[T](decisionName: String, 
               expected: T, 
               test: Resource => ReqRespData => Result[T],
               onSuccess: Int, 
               onFailure: Decision): Decision = apply(decisionName, expected, test, Left(setStatus(onSuccess)), Right(onFailure))
  
  def apply[T](decisionName: String,
                   expected: T,
                   test: Resource => ReqRespData => Result[T],
                   onSuccess: Either[Result[T] => ReqRespData, Decision],
                   onFailure: Either[Result[T] => ReqRespData, Decision]): Decision = apply(decisionName, test, (res: T, _: ReqRespData) => res == expected, onSuccess, onFailure)

  
  def apply[T](decisionName: String, test: Resource => ReqRespData => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Decision, onFailure: Int): Decision = apply(decisionName, test, check, onSuccess, setStatus(onFailure))
  
  def apply[T](decisionName: String, test: Resource => ReqRespData => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Decision, onFailure: Result[T] => ReqRespData): Decision = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))
  
  def apply[T](decisionName: String, test: Resource => ReqRespData => Result[T], check: (T, ReqRespData) => Boolean, onSuccess: Either[Result[T] => ReqRespData, Decision], onFailure: Either[Result[T] => ReqRespData, Decision]): Decision = new Decision {
    
    def name = decisionName
    
    def decide(resource: Resource, data: ReqRespData): (ReqRespData, Option[Decision]) = {
      val result = test(resource)(data)
      if (check(result.value, data)) handle(result, onSuccess)
      else handle(result, onFailure)
    }

    def handle(result: Result[T], handle: Either[Result[T] => ReqRespData, Decision]) = handle match {
      case Left(f) => (f(result), None)
      case Right(d) => (result.data, Some(d))
    }
    
  }
  
}

// methods are split up to allow stacking at start/end of flow as well as start/end of each decision
// run decision is pretty much extraneous outside of that
trait FlowRunnerBase {    

  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData 

  protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData): (ReqRespData, Option[Decision])

}

trait FlowRunner extends FlowRunnerBase {

  // can't mark this tail recursive if we want it to be stackable :(
  // can build an "optimized trait" later
  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData = {
    val (newData, mbNext) = runDecision(decision, resource, data)
    mbNext match {
      case Some(next) => run(next, resource, newData)
      case _ => newData
    }
  }

  protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData) = decision.decide(resource, data)

}