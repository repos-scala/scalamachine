package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import matcher.MatchResult
import mock._
import com.github.jrwest.scalamachine.core._
import flow.Decision


trait SpecsHelper {
  this: Specification with Mockito =>

  def createResource = mock[Resource]
  def createData(method: HTTPMethod = GET,
                 headers: Map[String,String] = Map(),
                 metadata: Metadata = Metadata(),
                 baseUri: String = "",
                 respHdrs: Map[String,String] = Map(),
                 respBody: HTTPBody = EmptyBody,
                 doRedirect: Boolean = false) =
    ReqRespData(
      baseUri = baseUri,
      method = method,
      requestHdrs = headers,
      responseHdrs = respHdrs,
      responseBody = respBody,
      metadata = metadata,
      doRedirect = doRedirect)

  def testDecision(decision: Decision,
                   stubF: Resource => Unit,
                   resource: Resource = createResource,
                   data: ReqRespData = createData())(f: (ReqRespData, Option[Decision]) => MatchResult[Any]): MatchResult[Any] = {
    // The issue with the failing tests is this stub call basically discards the changes we make to the data before the resource call.
    // one possible (maybe out there) way to resolve is move resources to being state actions (with a subclass that allows you to not need one if possible)
    // PROBABLY EASIER is to just stub the resource to return the data its given somehow and just used the passed in data above as the intial data to the decision
    stubF(resource) // make call to stub/mock
    val (mbNextDecision, newData) = decision(resource)(data)
    f(newData, mbNextDecision)
  }

  def testDecisionReturnsDecision(toTest: Decision,
                                  expectedDecision: Decision,
                                  stubF: Resource => Unit,
                                  resource: Resource = createResource,
                                  data: ReqRespData = createData()): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (_: ReqRespData, mbNextDecision: Option[Decision]) => mbNextDecision must beSome.like { case d => d must_== expectedDecision }
    }
  }

  def testDecisionReturnsDecisionAndData(toTest: Decision,
                                         expectedDecision: Decision,
                                         stubF: Resource => Unit,
                                         resource: Resource = createResource,
                                         data: ReqRespData = createData())(f: ReqRespData => MatchResult[Any]): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (data: ReqRespData, mbNextDecision: Option[Decision]) => mbNextDecision must beSome.which { _ == expectedDecision } and f(data)
    }
  }

  // test ReqRespData given no decision was returned
  def testDecisionReturnsData(toTest: Decision,
                              stubF: Resource => Unit,
                              resource: Resource = createResource,
                              data: ReqRespData = createData())(f: ReqRespData => MatchResult[Any]): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) => (mbNextDecision must beNone) and f(retData)
    }
  }

  // test ReqRespData regardless of whether a decision was returned
  def testDecisionResultHasData(toTest: Decision,
                                stubF: Resource => Unit,
                                resource: Resource = createResource,
                                data: ReqRespData = createData())(f: ReqRespData => MatchResult[Any]): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (retData: ReqRespData, _: Option[Decision]) => f(retData)
    }
  }

  def mkAnswer[T](value: T): Any => (Res[T], ReqRespData) = d => (ValueRes(value), d.asInstanceOf[ReqRespData])
  def mkResAnswer[T](value: Res[T]): Any => (Res[T], ReqRespData) = d => (value,d.asInstanceOf[ReqRespData])


}