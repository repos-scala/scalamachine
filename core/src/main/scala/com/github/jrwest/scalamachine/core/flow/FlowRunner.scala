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
  protected def runDecisionInner(resource: Resource, decision: Decision, data: ReqRespData): (Res[Any],ReqRespData,Option[Decision])

}



class FlowRunner extends FlowRunnerBase {

  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData = {
    runDecisionOuter(resource, decision, data) // tests pass but its wrong
    //runDecisionOuter2(resource, decision, data) // tests fail because mocking is off but its correct
  }

  def runDecisionOuter2(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData = {
    decision(resource)(data) match {
      case (Some(next), newData) => runDecisionOuter2(next, resource, newData)
      case (None, newData) => newData
    }
  }

  // can't mark this tail recursive if we want it to be stackable :(
  // can build an "optimized trait" later
  protected def runDecisionOuter(resource: Resource, decision: Decision, data: ReqRespData): ReqRespData = {
    runDecisionInner(resource, decision, data) match {
      case (ValueRes(_),newData, Some(nextDecision)) => runDecisionOuter(resource, nextDecision, newData)
      case (ErrorRes(_),newData, _) => newData.setStatusCode(500)
      case (HaltRes(code),newData, _) => newData.setStatusCode(code)
      case (_,newData, _) => newData
    }
  }

  protected def runDecisionInner(resource: Resource, decision: Decision, data: ReqRespData) = decision.decide(resource, data)
}