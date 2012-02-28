/*package com.github.jrwest.scalamachine.core.tests.old

import org.specs2._
import mock._
import com.github.jrwest.scalamachine.core._

class FlowRunnerSpec extends Specification with Mockito {
  def is =
    "ScalaMachine FlowRunner".title ^
      """
      The FlowRunner runs the WebMachine flow diagram
      for some given version. To implement the flow runner trait
      only the decisions method must be implemented. This spec is intended
      to test a generic webmachine flow not tied to any diagram.
      """ ^
      p ^
      "Given a non-empty list of decisions, FlowRunner.run(startData, startContext) should" ^
      "if a decision results in a regular result" ^
      "if the result matches the expected value" ^
      "its trueBranch is run next if its a decision" ! resultMatchingReturnDecision ^
      "the run terminates if trueFlow is an integer" ! resultMatchingReturnInt ^ p ^
      "if the result does not match the expected value" ^
      "its falseBranch is run next if its a decision" ! resultNotMatchingReturnDecision ^
      "the run terminates if falseFlow is an integer" ! resultNotMatchingReturnInt ^ p ^ p ^
      "if a decision results in an error" ^
      "no more decisions are run" ! skipped ^
      "if a decision results in a halt" ^
      "no more decisions are run" ! skipped ^
      end


  def resultMatchingReturnInt = {
    testDecisionFlow(tombstoneDecision, Result(true, _: ReqRespData, _: Context)) must
      haveTheSameElementsAs(List(tombstoneDecision, tombstoneDecision.trueBranch))
  }

  def resultMatchingReturnDecision = {
    val start = mkDecision(Right(tombstoneDecision), Left(402))
    val decisionResult = Result(true, _: ReqRespData, _: Context)
    testDecisionFlow(start, decisionResult) must
      haveTheSameElementsAs(List((start, Right(tombstoneDecision)), (tombstoneDecision, tombstoneDecision.trueBranch)))
  }

  def resultNotMatchingReturnInt = {
    testDecisionFlow(tombstoneDecision, Result(false, _: ReqRespData, _: Context)) must
      haveTheSameElementsAs(List(tombstoneDecision, tombstoneDecision.falseBranch))
  }

  def resultNotMatchingReturnDecision = {
    val start = mkDecision(Left(401), Right(tombstoneDecision))
    val decisionResult = Result(false, _: ReqRespData, _: Context)
    testDecisionFlow(start, decisionResult) must
      haveTheSameElementsAs(List((start, Right(tombstoneDecision)), (tombstoneDecision, tombstoneDecision.falseBranch)))
  }

  trait FlowTracker extends FlowRunnerBase {
    private var _steps = List[(Decision[Any], Either[Int, Decision[Any]])]()

    def steps = _steps.reverse

    abstract override protected def runDecision[T, C <: Context, A](r: Resource, data: ReqRespData, ctx: C, d: Decision[T]) = {
      val runResult = super.runDecision(r, data, ctx, d)
      _steps = (d, runResult._2) :: _steps
      runResult
    }
  }

  def createMockResource = mock[Resource]

  def createMockData = mock[ReqRespData]

  def tombstoneDecision = mkDecision(Left(401), Left(402))

  class TestFlow[T](start: Decision[T]) extends FlowRunner with FlowTracker {
    type SD = T

    protected def startDecision = start
  }


  def mkDecision[A, B](whenTrue: Either[Int, Decision[A]], whenFalse: Either[Int, Decision[B]]) =
    Decision(true, r => r.serviceAvailable(_, _), whenTrue, whenFalse)


  def testDecisionFlow(start: Decision[Boolean], resultF: (ReqRespData, Context) => DecisionResult[Boolean]): List[(Decision[Any], Either[Int, Decision[Any]])] = {
    val flowRunner = new TestFlow(start)
    val data = createMockData
    val resource = createMockResource
    val context = null // context is user only
    val result = resultF(data, context)

    resource.serviceAvailable(any, any) returns result
    flowRunner.run(resource, data, context)

    flowRunner.steps
  }


  // if decision throws an exception
  // if decision halts
  // if decision errors


}*/