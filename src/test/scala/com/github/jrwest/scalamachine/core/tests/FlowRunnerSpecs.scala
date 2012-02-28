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
  "Given a Decision which returns ReqRespData"                                      ^
    "the runner terminates running that decision last"                              ! testTerminatesWhenNoDecisionReturned ^
    "returns the data returned from the last decision"                              ! testReturnsUpdatedDataWhenTerminates ^
                                                                                    p^
  "Give a Decision which returns another decision"                                  ^
    "the runner runs the next decision"                                             ! testContinuesWithNextDecision ^
                                                                                    end
  
  trait TestFlowTracking extends FlowRunnerBase {
    var steps = List[(Decision, Either[ReqRespData,Decision])]()
    abstract override protected def runDecision(decision: Decision, resource: Resource, data: ReqRespData): (ReqRespData, Option[Decision]) = {
      val result = super.runDecision(decision, resource, data)
      val  (newData, nextDecision) = result
      steps = nextDecision.map((d: Decision) => (decision,Right(d))).getOrElse((decision,Left(newData))) :: steps
      result
    }
  }
  
  class TestFlow extends FlowRunner with TestFlowTracking

  def newFlow = new TestFlow 

  def decisionReturning(data: ReqRespData, next: Option[Decision]) = {
    val decision = mock[Decision]
    decision.decide(any, any) returns ((data, next))
    decision
  } 
  
  def testTerminatesWhenNoDecisionReturned = {    
    val returnedData = mock[ReqRespData]
    val decision = decisionReturning(returnedData, None)    
    val flow = newFlow
    flow.run(decision, mock[Resource], mock[ReqRespData])
    flow.steps must haveTheSameElementsAs((decision,Left(returnedData)) :: Nil)
  }

  def testReturnsUpdatedDataWhenTerminates = {    
    val startData = mock[ReqRespData]
    val returnedData = mock[ReqRespData]    
    val decision = decisionReturning(returnedData, None)
    newFlow.run(decision, mock[Resource], startData) must beEqualTo(returnedData) and not(beEqualTo(startData))
  }
  
  def testContinuesWithNextDecision = {
    val secondDecision = decisionReturning(mock[ReqRespData], None)
    val firstDecision = decisionReturning(mock[ReqRespData], Some(secondDecision))
    val flow = newFlow
    flow.run(firstDecision, mock[Resource], mock[ReqRespData])
    println (flow.steps)
    flow.steps.reverse.headOption must beSome.like {
      case (d1, Right(d2)) if d1 == firstDecision => d2 must beEqualTo(secondDecision)
    }
  }
  
}