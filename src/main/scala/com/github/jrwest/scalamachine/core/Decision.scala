package com.github.jrwest.scalamachine.core

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 2/27/12
 * Time: 11:15 PM 
 */

trait ReqRespData
trait Resource

trait Decision {

  def decide(resource: Resource, data: ReqRespData): (ReqRespData, Option[Decision])
  
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