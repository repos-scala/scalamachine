package com.github.jrwest.scalamachine.core
package flow

import scalaz.State


trait Decision {
  import Decision.FlowState

  def name: String

  def apply(resource: Resource): FlowState[Option[Decision]] = {
    import ReqRespData.statusCodeL
    for {
      res <- decide(resource)
      _ <- res match {
        case HaltRes(code) => statusCodeL := code
        case ErrorRes(_) => statusCodeL := 500
        case _ => statusCodeL.st
      }
    } yield res.toOption
  }

  protected def decide(resource: Resource): FlowState[Res[Decision]]

  override def equals(o: Any): Boolean = o match {
    case o: Decision => o.name == name
    case _ => false
  }

  override def toString = "Decision(%s)" format name

}

object Decision {
  import ReqRespData.statusCodeL

  import scalaz.syntax.pointed._
  type FlowState[T] = State[ReqRespData, T]
  type ResourceF[T] = Resource => ReqRespData => (Res[T], ReqRespData)
  type CheckF[T] = (T, ReqRespData) => Boolean
  type HandlerF[T] = (T,ReqRespData) => ReqRespData
  type Handler[T] = Either[HandlerF[T],Decision]

  private[this] def setStatus[T](code: Int): HandlerF[T] = (_,d) => (statusCodeL := code) exec d

  def apply[T](decisionName: String, test: ResourceF[T], check: CheckF[T], onSuccess: Handler[T], onFailure: Handler[T]) = new Decision {
    def name: String = decisionName

    protected def decide(resource: Resource): FlowState[Res[Decision]] = {
      val nextT: ResT[FlowState,Decision] = for {
        value <- ResT[FlowState,T](State((d: ReqRespData) => test(resource)(d)))
        handler <- ResT[FlowState,Handler[T]](State((d: ReqRespData) => if (check(value, d)) (onSuccess.point[Res],d) else (onFailure.point[Res], d)))
        next <- ResT[FlowState,Decision](State((d: ReqRespData) => handler match {
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

}
