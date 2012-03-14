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
    "if the result is a SimpleResult, the runner runs the next decision"            ! testContinuesWithNextDecisionOnSimpleResult ^
    "if the result is a ErrorResult"                                                ^
      "the next decision is not run"                                                ! testErrorResultDoesNotRunNextDecision ^
      "the resulting data is returned with the response"                            ^
        "having code 500"                                                           ! testErrorResultReturns500Response ^
        "body set to the value returned as part of the result"                      ! skipped ^p^p^
    "if the result is a HaltResult"                                                 ^
       "the next decision is not run"                                               ! testHaltResultDoesNotRunNextDecision ^
       "the returned data has response code matching HaltResult code"               ! testHaltResultReturnsResponseWithHaltCode ^
                                                                                    p^p^
                                                                                    end

  trait TestFlowTracking[C] extends FlowRunnerBase[C] {
    var steps = List[(Decision[C], Either[Result[C,Any],Decision[C]])]()
    abstract override protected def runDecisionInner(resource: Resource[C], decision: Decision[C], data: ReqRespData, ctx: C) = {
      val result = super.runDecisionInner(resource, decision, data,ctx)
      val  (res, nextDecision) = result
      steps = nextDecision.map((d: Decision[C]) => (decision,Right(d))).getOrElse((decision,Left(res))) :: steps
      result
    }
  }

  // we don't care about the context in these tests so lets make it whatever we want
  type TestContext = Int
  def createDummyContext: TestContext = 0

  class TestFlow extends FlowRunner[TestContext] with TestFlowTracking[TestContext]

  def newFlow = new TestFlow
  def createResult(data: ReqRespData = mock[ReqRespData], ctx: TestContext = createDummyContext, value: Any = null): Result[TestContext,Any] =
    SimpleResult(value, data, ctx)

  def decisionReturning(result: Result[TestContext,Any], next: Option[Decision[TestContext]]) = {
    val decision = mock[Decision[TestContext]]
    decision.decide(any, any, any) returns ((result, next))
    decision
  } 
  
  def testTerminatesWhenNoDecisionReturned = {    
    val returnedResult = createResult()
    val decision = decisionReturning(returnedResult, None)
    val flow = newFlow
    flow.run(decision, mock[Resource[TestContext]], mock[ReqRespData])
    flow.steps must haveTheSameElementsAs((decision,Left(returnedResult)) :: Nil)
  }

  def testReturnsUpdatedDataWhenTerminates = {
    val startData = mock[ReqRespData]
    val endData = mock[ReqRespData]
    val returnedResult = createResult(data = endData)
    val decision = decisionReturning(returnedResult, None)
    newFlow.run(decision, mock[Resource[TestContext]], startData) must
      beEqualTo(endData) and not(beEqualTo(startData))
  }
  
  def testContinuesWithNextDecisionOnSimpleResult = {
    val secondDecision = decisionReturning(createResult(), None)
    val firstDecision = decisionReturning(createResult(), Some(secondDecision))
    val flow = newFlow
    flow.run(firstDecision, mock[Resource[TestContext]], mock[ReqRespData])
    (flow.steps.size must beEqualTo(2)) and (flow.steps.reverse.headOption must beSome.like {
      case (d1, Right(d2)) if d1 == firstDecision => d2 must beEqualTo(secondDecision)
    })
  }

  def testErrorResultDoesNotRunNextDecision = {
    val secondDecision = decisionReturning(createResult(), None)
    val firstDecision = decisionReturning(ErrorResult(null,mock[ReqRespData],createDummyContext), Some(secondDecision))
    val flow = newFlow
    flow.run(firstDecision, mock[Resource[TestContext]], mock[ReqRespData])
    flow.steps.size must beEqualTo(1)
  }
  
  def testErrorResultReturns500Response = {
    val secondDecision = decisionReturning(createResult(), None)
    val firstDecision = decisionReturning(ErrorResult(null,ImmutableReqRespData(GET),createDummyContext), Some(secondDecision))
    val flow = newFlow
    val result = flow.run(firstDecision, mock[Resource[TestContext]], ImmutableReqRespData(GET))
    result.statusCode must beEqualTo(500)
  }
  
  def testHaltResultDoesNotRunNextDecision = {
    val secondDecision = decisionReturning(createResult(), None)
    val firstDecision = decisionReturning(HaltResult(400,mock[ReqRespData],createDummyContext), Some(secondDecision))
    val flow = newFlow
    flow.run(firstDecision, mock[Resource[TestContext]], mock[ReqRespData])
    flow.steps.size must beEqualTo(1)
  }
  
  def testHaltResultReturnsResponseWithHaltCode = {
    val code = 401
    val secondDecision = decisionReturning(createResult(), None)
    val firstDecision = decisionReturning(HaltResult(code,ImmutableReqRespData(GET),createDummyContext), Some(secondDecision))
    val flow = newFlow
    val result = flow.run(firstDecision, mock[Resource[TestContext]], ImmutableReqRespData(GET))
    result.statusCode must beEqualTo(code)
  }

}