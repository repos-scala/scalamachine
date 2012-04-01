package com.github.jrwest.scalamachine.core
package flow


trait Decision {


  def name: String

  // we use any for the type parameterizing Result because the value is not required at higher levels
  def decide(resource: Resource, data: ReqRespData): (Res[Any], ReqRespData, Option[Decision])

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

}

object Decision {
  import ReqRespData.statusCodeL

  type CheckF[T] = (T, ReqRespData) => Boolean
  type ResourceF[T] = Resource => ReqRespData => (Res[T], ReqRespData)
  type HandleF[T] = (ValueRes[T],ReqRespData) => ReqRespData
  type Handler[T] = Either[HandleF[T],Decision]

  private[this] def setStatus[T](code: Int): HandleF[T] = (_,d) => (statusCodeL := code) exec d

  def apply[T](decisionName: String, test: ResourceF[T], check: CheckF[T], onSuccess: Handler[T], onFailure: Handler[T]): Decision = new Decision {

    def name = decisionName

    def decide(resource: Resource, data: ReqRespData): (Res[Any], ReqRespData, Option[Decision]) = {
      test(resource)(data) match {
        case (result @ ValueRes(value),newData) =>
          if (check(value,newData)) handle(result,newData,onSuccess)
          else handle(result,newData,onFailure)
        case (result,newData) => (result, newData,None)
      }
    }

    def handle(result: ValueRes[T], data: ReqRespData, handle: Either[(ValueRes[T],ReqRespData) => ReqRespData, Decision]): (Res[Any], ReqRespData, Option[Decision]) = handle match {
      case Left(f) => (result,f(result,data),None)//((resultDataL := f(result)) exec result, None)
      case Right(d) => (result, data, Some(d)) //(result, Some(d))
    }

  }


  def apply[T](decisionName: String,
               expected: T,
               test: ResourceF[T],
               onSuccess: Decision,
               onFailure: Int): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(setStatus[T](onFailure)))

  def apply[T](decisionName: String,
               expected: T,
               test: ResourceF[T],
               onSuccess: Int,
               onFailure: Decision): Decision = apply(decisionName, expected, test, Left(setStatus[T](onSuccess)), Right(onFailure))

  def apply[T](decisionName: String,
               expected: T,
               test: ResourceF[T],
               onSuccess: Decision,
               onFailure: HandleF[T]): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(onFailure))

  def apply[T](decisionName: String,
               expected: T,
               test: ResourceF[T],
               onSuccess: Handler[T],
               onFailure: Handler[T]): Decision = apply(decisionName, test, (res: T, _: ReqRespData) => res == expected, onSuccess, onFailure)


  def apply[T](decisionName: String,
               test: ResourceF[T],
               check: CheckF[T],
               onSuccess: Decision,
               onFailure: Int): Decision = apply(decisionName, test, check, onSuccess, setStatus[T](onFailure))

  def apply[T](decisionName: String,
               test: ResourceF[T],
               check: CheckF[T],
               onSuccess: Decision,
               onFailure: HandleF[T]): Decision = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))

}
