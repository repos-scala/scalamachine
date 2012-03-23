package com.github.jrwest.scalamachine.core
package flow

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
    runDecisionOuter(resource, decision, data)
  }

  // can't mark this tail recursive if we want it to be stackable :(
  // can build an "optimized trait" later
  protected def runDecisionOuter(resource: Resource, decision: Decision, data: ReqRespData): ReqRespData = {
    runDecisionInner(resource, decision, data) match {
      case (SimpleResult(_, newData), Some(nextDecision)) => runDecisionOuter(resource, nextDecision, newData)
      case (ErrorResult(_, newData), _) => newData.setStatusCode(500)
      case (HaltResult(code, newData), _) => newData.setStatusCode(code)
      case (result, _) => result.data
    }
  }

  protected def runDecisionInner(resource: Resource, decision: Decision, data: ReqRespData) = decision.decide(resource, data)
}