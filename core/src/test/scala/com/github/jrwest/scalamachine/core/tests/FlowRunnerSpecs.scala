package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import execute.Success
import mock._
import com.github.jrwest.scalamachine.core._
import flow._


class FlowRunnerSpecs extends Specification with Mockito { def is =
  "FlowRunner".title                                                                ^
  """
  The FlowRunner is responsible for running the webmachine flow
  for a given version. It is given a start decision and keeps running
  each decision until it is given a response instead.
  """                                                                               ^
                                                                                    p^
  "Given a Decision returns no next decision"                                       ^
    "the runner terminates running that decision last"                              ! testTerminatesWhenNoDecisionReturned ^
    "returns the resulting data from the last decision"                             ! testTerminationReturnsCorrectData ^
  "Given a decision returns another decision"                                       ^
    "the runner runs the next decision passing in the resulting data"               ! testContinuesWhenGivenNextDecision ^
                                                                                    end

  trait TestFlowTracking extends FlowRunnerBase {
    var steps = List[(Decision, (Option[Decision], ReqRespData))]()
    abstract override protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData): (Option[Decision], ReqRespData) = {
      val result = super.runDecision(decision, resource, data)
      steps ::= (decision, result)
      result
    }
  }

  def newFlow = new FlowRunner with TestFlowTracking

  import scalaz.StateT
  import scalaz.State
  def decisionReturning(next: Option[Decision], data: ReqRespData): Decision = {
    val decision = mock[Decision]
    decision.apply(any) returns (State((d: ReqRespData) => (next, data)))
    decision
  }

  def testTerminatesWhenNoDecisionReturned = {
    val (startData, endData) = (mock[ReqRespData], mock[ReqRespData])
    val decision = decisionReturning((None: Option[Decision]), endData)
    val flow = newFlow
    flow.run(decision, mock[Resource], startData)
    flow.steps.unzip._1 aka "the list of run decisions" must haveTheSameElementsAs(decision :: Nil)
  }

  def testTerminationReturnsCorrectData = {
    val (startData, endData) = (mock[ReqRespData], mock[ReqRespData])
    val decision = decisionReturning((None: Option[Decision]), endData)
    val flow = newFlow
    val data = flow.run(decision, mock[Resource], startData)
    data must beEqualTo(endData)
  }

  def testContinuesWhenGivenNextDecision = {
    val firstStepData = mock[ReqRespData]
    val lastStepData = mock[ReqRespData]
    val endDecision = decisionReturning((None: Option[Decision]), lastStepData)
    val firstDecision = decisionReturning(Option(endDecision), firstStepData)
    val flow = newFlow
    val finalData = flow.run(firstDecision, mock[Resource], mock[ReqRespData])
    (finalData must beEqualTo(lastStepData)) and (flow.steps.reverse.headOption must beSome.like {
      case (decision, (Some(next), data)) =>
        (decision must beEqualTo(firstDecision)) and
          (next must beEqualTo(endDecision)) and
          (data must beEqualTo(firstStepData))
    })
  }

}
