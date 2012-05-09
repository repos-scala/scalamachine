package com.github.jrwest.scalamachine.core
package flow

import scalaz.State


trait Decision {


  def name: String

  def apply(resource: Resource): State[ReqRespData, Option[Decision]] = {
    import ReqRespData.statusCodeL
    for {
      res <- decide2(resource)
      _ <- res match {
        case HaltRes(code) => statusCodeL := code
        case ErrorRes(_) => statusCodeL := 500
        case _ => statusCodeL.st
      }
    } yield res.toOption
  }

  // we use any for the type parameterizing Result because the value is not required at higher levels
  // TODO: remove, this is a hack to get things to compile
  def decide(resource: Resource, data: ReqRespData): (Res[Any], ReqRespData, Option[Decision]) = {
    val (res, d) = decide2(resource)(data)
    (res,d,res.toOption)
  }

  def decide2(resource: Resource): State[ReqRespData, Res[Decision]]

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

  override def toString = "Decision(%s)" format name

}

object Decision {
  import ReqRespData.statusCodeL

  import scalaz.syntax.pointed._
  type ResourceF[T] = Resource => ReqRespData => (Res[T], ReqRespData)
  type CheckF[T] = (T, ReqRespData) => Boolean
  type HandlerF[T] = (T,ReqRespData) => ReqRespData
  type Handler[T] = Either[HandlerF[T],Decision]

  private[this] def setStatus[T](code: Int): HandlerF[T] = (_,d) => (statusCodeL := code) exec d

  def apply[T](decisionName: String, test: ResourceF[T], check: CheckF[T], onSuccess: Handler[T], onFailure: Handler[T]) = new Decision {
    def name: String = decisionName

    override def decide2(resource: Resource): State[ReqRespData, Res[Decision]] = {
      type S[X] = State[ReqRespData,X]
      val nextT: ResT[S,Decision] = for {
        value <- ResT[S,T](State((d: ReqRespData) => test(resource)(d)))
        handler <- ResT[S,Handler[T]](State((d: ReqRespData) => if (check(value, d)) (onSuccess.point[Res],d) else (onFailure.point[Res], d)))
        next <- ResT[S,Decision](State((d: ReqRespData) => handler match {
          case Left(f) => ((EmptyRes: Res[Decision]), f(value, d))
          case Right(decision) => (ValueRes(decision), d)
        }))
      } yield next

      nextT.run
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
               onFailure: HandlerF[T]): Decision = apply(decisionName, expected, test, Right(onSuccess), Left(onFailure))

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
               onFailure: HandlerF[T]): Decision = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))




  /*type CheckF[T] = (T, ReqRespData) => Boolean
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
               onFailure: HandleF[T]): Decision = apply(decisionName, test, check, Right(onSuccess), Left(onFailure))*/

}
