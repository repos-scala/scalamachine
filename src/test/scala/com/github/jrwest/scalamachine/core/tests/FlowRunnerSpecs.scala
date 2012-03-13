package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import mock._
import com.github.jrwest.scalamachine.core._

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 2/27/12
 * Time: 11:12 PM
 */

class FlowRunnerSpecs extends Specification with Mockito { def is =
  "FlowRunner".title                                                                ^
  """
  The FlowRunner is responsible for running the webmachine flow
  for a given version. It is given a start decision and keeps running
  each decision until it is given a response instead.
  """                                                                               ^
                                                                                    p^
  "Given a Decision which returns a result but not another decision"                ^
    "the runner terminates running that decision last"                              ! testTerminatesWhenNoDecisionReturned ^
    "returns the result returned from the last decision"                            ! testReturnsUpdatedDataWhenTerminates ^
                                                                                    p^
  "Give a Decision which returns another decision"                                  ^
    "the runner runs the next decision"                                             ! testContinuesWithNextDecision ^
                                                                                    end

  trait TestFlowTracking extends FlowRunnerBase {
    var steps = List[(Decision, Either[Result[Any],Decision])]()
    abstract override protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData, ctx: Context) = {
      val result = super.runDecision(decision, resource, data,ctx)
      val  (res, nextDecision) = result
      steps = nextDecision.map((d: Decision) => (decision,Right(d))).getOrElse((decision,Left(res))) :: steps
      result
    }
  }
  
  class TestFlow extends FlowRunner with TestFlowTracking

  def newFlow = new TestFlow
  def createResult(data: ReqRespData = mock[ReqRespData], ctx: Context = mock[Context], value: Any = null): Result[Any] = SimpleResult(value, data, ctx)

  def decisionReturning(result: Result[Any], next: Option[Decision]) = {
    val decision = mock[Decision]
    decision.decide(any, any, any) returns ((result, next))
    decision
  } 
  
  def testTerminatesWhenNoDecisionReturned = {    
    val returnedResult = createResult()
    val decision = decisionReturning(returnedResult, None)
    val flow = newFlow
    flow.run(decision, mock[Resource], mock[ReqRespData], mock[Context])
    flow.steps must haveTheSameElementsAs((decision,Left(returnedResult)) :: Nil)
  }

  def testReturnsUpdatedDataWhenTerminates = {
    val startData = mock[ReqRespData]
    val endData = mock[ReqRespData]
    val returnedResult = createResult(data = endData)
    val decision = decisionReturning(returnedResult, None)
    newFlow.run(decision, mock[Resource], startData, mock[Context]) must
      beEqualTo(endData) and not(beEqualTo(startData))
  }
  
  def testContinuesWithNextDecision = {
    val secondDecision = decisionReturning(createResult(), None)
    val firstDecision = decisionReturning(createResult(), Some(secondDecision))
    val flow = newFlow
    flow.run(firstDecision, mock[Resource], mock[ReqRespData], mock[Context])
    flow.steps.reverse.headOption must beSome.like {
      case (d1, Right(d2)) if d1 == firstDecision => d2 must beEqualTo(secondDecision)
    }
  }
  
}