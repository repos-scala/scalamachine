package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import matcher.MatchResult
import mock._
import com.github.jrwest.scalamachine.core._

/**
 * Created by IntelliJ IDEA.
 * User: jordanrw
 * Date: 2/28/12
 * Time: 1:20 AM
 */

class WebmachineV3Specs extends Specification with Mockito with WebmachineDecisions { def is = ""            ^
  "WebMachine V3".title                                                             ^
  """
  The WebMachine Version 3 Flow

  http://wiki.basho.com/images/http-headers-status-v3.png
  """                                                                               ^
                                                                                    p^
  "B13 - Service Available?"                                                        ^
    "asks the resource if the service is available"                                 ^
      "if it is, decision B12 is returned"                                          ! testServiceAvailTrue ^
      "if it is not, a response with code 503 is returned"                          ! testServiceAvailFalse ^
                                                                                    p^p^
  "B12 - Known Method?"                                                             ^
    "asks the resource for the list of known methods"                               ^
      "if the request method is in the list, decision B11 is returned"              ! testKnownMethodTrue ^
      "if it is not, a response with code 501 is returned"                          ! testKnownMethodFalse ^
                                                                                    p^p^
  "B11 - URI too long?"                                                             ^
    "asks the resource if the request uri is too long"                              ^
      "if it is not, decision b10 is returned"                                      ! testURITooLongFalse ^
      "if it is, a response with code 414 is returned"                              ! testURITooLongTrue ^
                                                                                    p^p^
  "B10 - Allowed Method?"                                                           ^
    "asks resource for list of allowed methods"                                     ^
      "if request method is contained in allowed methods, decision B9 is returned"  ! testAllowedMethodTrue ^
      "if request method is not contained in allowed methods, a response"           ^
        "with code 405 is returned"                                                 ! testAllowedMethodFalseRespCode ^
        "with Allow header set to comma-sep list of allowed methods from resource"  ! skipped ^
                                                                                    p^p^p^
  "B9 - Malformed Request?"                                                         ^
    "asks resource if request is malformed"                                         ^
      "if it is not, decision b8 is returned"                                       ! testMalformedFalse ^
      "if it is, a response with code 400 is returned"                              ! testMalformedTrue ^
                                                                                    p^p^
  "B8 - Authorized"                                                                 ^
    "asks resource if request is authorized"                                        ^
      "if it is, decision B7 is returned"                                           ! skipped ^
      "if it is not, a response"                                                    ^
        "with code 401 is returned"                                                 ! skipped ^
        "with the WWW-Authenticate header not set if resource result was a halt"    ! skipped ^
        "with the WWW-Authenticate header not set if the resource result was error" ! skipped ^
        "with the WWW-Authenticate header set to value returned by resource"        ! skipped ^
                                                                                    p^p^p^
  "B7 - Forbidden?"                                                                 ^
    "asks resource if request is forbidden"                                         ^
      "if it is not, decision B6 is returned"                                       ! skipped ^
      "if it is, a response with code 403 is returned"                              ! skipped ^
                                                                                    p^p^
  "B6 - Valid Content-* Headers?"                                                   ^
    "asks resource if content headers are valid"                                    ^
      "if they are, decision B5 is returned"                                        ! skipped ^
      "if they are not, a response with code 501 is returned"                       ! skipped ^
                                                                                    p^p^
  "B5 - Known Content Type?"                                                        ^
    "asks resource if the Content-Type is known"                                    ^
      "if it is, decision B4 is returned"                                           ! skipped ^
      "if it is not, a response with code 415 is returned"                          ! skipped ^
                                                                                    p^p^
  "B4 - Request Entity Too Large?"                                                  ^
    "asks resource if the request entity length is valid"                           ^
      "if it is, decision B3 is returned"                                           ! skipped ^
      "if it is not, a response with code 413 is returned"                          ! skipped ^
                                                                                    p^p^
  "B3 - OPTIONS?"                                                                   ^
    "if the request method is OPTIONS"                                              ^
      "a response with code 200 is returned"                                        ! skipped ^
      "otherwise, decision C3 is returned"                                          ! skipped ^
                                                                                    p^p^
                                                                                    end
  def createResource = mock[Resource]
  def createData(method: HTTPMethod = GET) = ImmutableReqRespData(method = method)  

  def testDecision(decision: Decision,
                   stubF: (Resource, ReqRespData) => Unit,
                   resource: Resource = createResource, 
                   data: ReqRespData = createData())(f: (ReqRespData, Option[Decision]) => MatchResult[Any]): MatchResult[Any] = {
    stubF(resource, data) // make call to stub/mock
    val (retData, mbNextDecision) = decision.decide(resource, data)
    f(retData, mbNextDecision)
  }

  def testDecisionReturnsDecision(toTest: Decision,
                                  expectedDecision: Decision,
                                  stubF: (Resource, ReqRespData) => Unit,
                                  resource: Resource = createResource,
                                  data: ReqRespData = createData()): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (_: ReqRespData, mbNextDecision: Option[Decision]) => mbNextDecision must beSome.which { _ == expectedDecision }
    }
  }
  def testServiceAvailTrue = {
    testDecisionReturnsDecision(b13, b12, (r,d) => r.serviceAvailable(any) returns SimpleResult(true,d))
  }

  def testServiceAvailFalse = {
    testDecision(b13, (r,d) => r.serviceAvailable(any) returns SimpleResult(false, d)) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) =>
        (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(503))
    }
  }

  def testKnownMethodTrue = {
    testDecisionReturnsDecision(b12, b11, (r,d) => r.knownMethods(any) returns SimpleResult(List(GET,POST), d))
  }
  
  def testKnownMethodFalse = {
    testDecision(b12, (r,d) => r.knownMethods(any) returns SimpleResult(List(GET),d), data = createData(method = POST)) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) =>
        (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(501))
    }
  }
  
  def testURITooLongFalse = {
    testDecisionReturnsDecision(b11, b10, (r,d) => r.uriTooLong(any) returns SimpleResult(false,d))
  }
  
  def testURITooLongTrue = {
    testDecision(b11, (r,d) => r.uriTooLong(any) returns SimpleResult(true,d)) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) =>
        (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(414))
    }
  }

  def testAllowedMethodTrue = {
    testDecisionReturnsDecision(b10, b9, (r,d) => r.allowedMethods(any) returns SimpleResult(List(GET,POST),d))
  }

  def testAllowedMethodFalseRespCode = {
    testDecision(b10,(r,d) => r.allowedMethods(any) returns SimpleResult(List(GET,DELETE),d), data = createData(method = POST)) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) =>
        (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(405))
    }
  }
  
  def testMalformedFalse = {
    testDecisionReturnsDecision(b9, b8, (r,d) => r.isMalformed(any) returns SimpleResult(false,d))
  }

  def testMalformedTrue = {
    testDecision(b9,(r,d) => r.isMalformed(any) returns SimpleResult(true,d)) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) =>
        (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(400))
    }
  }
  
}