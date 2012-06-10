package com.github.jrwest.scalamachine.core
package flow

import scalaz.State
import org.slf4j.{LoggerFactory, Logger}


// methods are split up to allow stacking at start/end of flow as well as start/end of each decision
// run decision is pretty much extraneous outside of that
trait FlowRunnerBase {

  // stacking this method gives access to start/end of flow
  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData

  protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData): (ReqRespData,Option[Decision])
}



class FlowRunner extends FlowRunnerBase {

  // TODO: "finish the response" https://github.com/basho/webmachine/blob/master/src/webmachine_decision_core.erl#L65-99
  def run(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData = {
    // loop is here so stacked traits can tie into run using it as a entry and exit point for
    // the entire flow <<-- TODO: except for this is false
    def loop(decision: Decision, resource: Resource, data: ReqRespData): ReqRespData = {
      runDecision(decision, resource, data) match {
        case (newData,Some(next)) => run(next, resource, newData)
        case (newData,None) => newData
      }
    }
    loop(decision, resource, data)
  }

  protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData): (ReqRespData,Option[Decision]) =
    decision(resource)(data)
}

trait FlowLogging extends FlowRunnerBase {

  private val logger: Logger = LoggerFactory.getLogger(classOf[FlowRunner])

  abstract override protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData): (ReqRespData,Option[Decision]) = {
    logger.debug("running decision %s with data: %s" format (decision.name, data))
    val result = super.runDecision(decision, resource, data)
    logger.debug("finished running decision %s" format decision.name)
    result
  }
}