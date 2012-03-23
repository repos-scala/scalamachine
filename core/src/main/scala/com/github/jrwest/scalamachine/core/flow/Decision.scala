package com.github.jrwest.scalamachine.core
package flow


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


trait Decision {


  def name: String

  // we use any for the type parameterizing Result because the value is not required at higher levels
  def decide(resource: Resource, data: ReqRespData): (Result[Any], Option[Decision])

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

}