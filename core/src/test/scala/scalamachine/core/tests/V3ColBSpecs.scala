package scalamachine.core.tests

import org.specs2._
import matcher.MatchResult
import mock._
import org.mockito.{Matchers => MM}
import scalamachine.core._
import Resource._
import flow._
import v3.WebmachineDecisions
import scalamachine.internal.scalaz.NonEmptyList
import org.apache.commons.httpclient.util.DateUtil
import HTTPHeaders._
import HTTPMethods._


class V3ColBSpecs extends Specification with Mockito with SpecsHelper with WebmachineDecisions { def is =
  "Webmachine V3 Column B".title                                                    ^
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
        "with Allow header set to comma-sep list of allowed methods from resource"  ! testAllowedMethodFalseAllowHeader ^
                                                                                    p^p^p^
  "B9 - Malformed Request?"                                                         ^
    "asks resource if request is malformed"                                         ^
      "if it is not, decision b8 is returned"                                       ! testMalformedFalse ^
      "if it is, a response with code 400 is returned"                              ! testMalformedTrue ^
                                                                                    p^p^
  "B8 - Authorized"                                                                 ^
    "asks resource if request is authorized"                                        ^
      "if it is, decision B7 is returned"                                           ! testAuthTrue ^
      "if it is not, a response"                                                    ^
        "with code 401 is returned"                                                 ! testAuthFalseRespCode ^
        "with the WWW-Authenticate header not set if resource result was a halt"    ! testAuthFalseHaltResult ^
        "with the WWW-Authenticate header not set if the resource result was error" ! testAuthFalseErrorResult ^
        "with the WWW-Authenticate header set to value returned by resource"        ! testAuthFalseAuthHeader ^
                                                                                    p^p^p^
  "B7 - Forbidden?"                                                                 ^
    "asks resource if request is forbidden"                                         ^
      "if it is not, decision B6 is returned"                                       ! testForbiddenFalse ^
      "if it is, a response with code 403 is returned"                              ! testForbiddenTrue ^
                                                                                    p^p^
  "B6 - Valid Content-* Headers?"                                                   ^
    "asks resource if content headers are valid"                                    ^
      "if they are, decision B5 is returned"                                        ! testValidContentHeadersTrue ^
      "if they are not, a response with code 501 is returned"                       ! testValidContentHeadersFalse ^
                                                                                    p^p^
  "B5 - Known Content Type?"                                                        ^
    "asks resource if the Content-Type is known"                                    ^
      "if it is, decision B4 is returned"                                           ! testKnownContentTypeTrue ^
      "if it is not, a response with code 415 is returned"                          ! testKnownContentTypeFalse ^
                                                                                    p^p^
  "B4 - Request Entity Too Large?"                                                  ^
    "asks resource if the request entity length is valid"                           ^
      "if it is, decision B3 is returned"                                           ! testIsValidEntityLengthTrue ^
      "if it is not, a response with code 413 is returned"                          ! testIsValidEntityLengthFalse ^
                                                                                    p^p^
  "B3 - OPTIONS?"                                                                   ^
    "if the request method is OPTIONS"                                              ^
      "a response with code 200 is returned"                                        ! testRequestIsOptions ^
      "response has headers returned by Resource.options"                           ! testRequestIsOptionsUsesResourceOptionsHeaders ^p^
    "otherwise, decision C3 is returned"                                            ! testRequestIsNotOptions ^
                                                                                    end

  def testServiceAvailTrue = {
    testDecisionReturnsDecision(b13, b12, _.serviceAvailable(any) answers mkAnswer(true))
  }

  def testServiceAvailFalse = {
    testDecisionReturnsData(b13, _.serviceAvailable(any) answers mkAnswer(false)) {
      _.statusCode must beEqualTo(503)
    }
  }

  def testKnownMethodTrue = {
    testDecisionReturnsDecision(b12, b11, _.knownMethods(any) answers mkAnswer(List(GET,POST)))
  }

  def testKnownMethodFalse = {
    testDecisionReturnsData(b12, _.knownMethods(any) answers mkAnswer(List(GET)), data = createData(method = POST)) {
      _.statusCode must beEqualTo(501)
    }
  }

  def testURITooLongFalse = {
    testDecisionReturnsDecision(b11, b10, _.uriTooLong(any) answers mkAnswer(false))
  }

  def testURITooLongTrue = {
    testDecisionReturnsData(b11, _.uriTooLong(any) answers mkAnswer(true)) {
      _.statusCode must beEqualTo(414)
    }
  }

  def testAllowedMethodTrue = {
    testDecisionReturnsDecision(b10, b9, _.allowedMethods(any) answers mkAnswer(List(GET,POST)))
  }

  def testAllowedMethodFalseRespCode = {
    testDecisionReturnsData(b10,_.allowedMethods(any) answers mkAnswer(List(GET,DELETE)), data = createData(method = POST)) {
      _.statusCode must beEqualTo(405)
    }
  }

  def testAllowedMethodFalseAllowHeader = {
    testDecisionReturnsData(b10, _.allowedMethods(any) answers mkAnswer(List(GET,POST,DELETE)), data = createData(method=PUT)) {
      _.responseHeader(Allow) must beSome.like {
        case s => s must contain("GET") and contain("POST") and contain("DELETE") // this could be improved (use the actual list above)
      }
    }
  }

  def testMalformedFalse = {
    testDecisionReturnsDecision(b9, b8, _.isMalformed(any) answers mkAnswer(false))
  }

  def testMalformedTrue = {
    testDecisionReturnsData(b9,_.isMalformed(any) answers mkAnswer(true)) {
      _.statusCode must beEqualTo(400)
    }
  }

  def testAuthTrue = {
    testDecisionReturnsDecision(b8, b7, _.isAuthorized(any) answers mkAnswer(AuthSuccess))
  }

  def testAuthFalseRespCode = {
    testDecisionReturnsData(b8,_.isAuthorized(any) answers mkAnswer(AuthFailure("something"))) {
      _.statusCode must beEqualTo(401)
    }
  }

  def testAuthFalseHaltResult = {
    testDecisionReturnsData(b8, _.isAuthorized(any) answers mkResAnswer(HaltRes(500))) {
      _.responseHeader(WWWAuthenticate) must beNone
    }
  }

  def testAuthFalseErrorResult = {
    testDecisionReturnsData(b8, _.isAuthorized(any) answers mkResAnswer(ErrorRes(""))) {
      _.responseHeader(WWWAuthenticate) must beNone
    }
  }

  def testAuthFalseAuthHeader = {
    val headerValue = "somevalue"
    testDecisionReturnsData(b8, _.isAuthorized(any) answers mkAnswer(AuthFailure(headerValue))) {
      _.responseHeader(WWWAuthenticate) must beSome.which { _ == headerValue }
    }
  }

  def testForbiddenFalse = {
    testDecisionReturnsDecision(b7,b6,_.isForbidden(any) answers mkAnswer(false))
  }

  def testForbiddenTrue = {
    testDecisionReturnsData(b7,_.isForbidden(any) answers mkAnswer(true)) {
      _.statusCode must beEqualTo(403)
    }
  }

  def testValidContentHeadersTrue = {
    testDecisionReturnsDecision(b6,b5,_.contentHeadersValid(any) answers mkAnswer(true))
  }

  def testValidContentHeadersFalse = {
    testDecisionReturnsData(b6,_.contentHeadersValid(any) answers mkAnswer(false)) {
      _.statusCode must beEqualTo(501)
    }
  }

  def testKnownContentTypeTrue = {
    testDecisionReturnsDecision(b5,b4,_.isKnownContentType(any) answers mkAnswer(true))
  }

  def testKnownContentTypeFalse = {
    testDecisionReturnsData(b5,_.isKnownContentType(any) answers mkAnswer(false)) {
      _.statusCode must beEqualTo(415)
    }
  }

  def testIsValidEntityLengthTrue = {
    testDecisionReturnsDecision(b4,b3,_.isValidEntityLength(any) answers mkAnswer(true))
  }

  def testIsValidEntityLengthFalse = {
    testDecisionReturnsData(b4,_.isValidEntityLength(any) answers mkAnswer(false)) {
      _.statusCode must beEqualTo(413)
    }
  }

  def testRequestIsOptions = {
    testDecisionReturnsData(b3,_.options(any) answers mkAnswer(Map[HTTPHeader,String]()), data = createData(method=OPTIONS)) {
      _.statusCode must beEqualTo(200)
    }
  }

  def testRequestIsOptionsUsesResourceOptionsHeaders = {
    val XA = createHeader("X-A")
    val XB = createHeader("X-B")
    val testHeaders =  Map(XA -> "a", XB -> "b")
    testDecisionReturnsData(b3,_.options(any) answers mkAnswer(testHeaders), data = createData(method=OPTIONS)) {
      _.responseHeaders must containAllOf(testHeaders.toList)
    }
  }

  def testRequestIsNotOptions = {
    testDecisionReturnsDecision(b3,c3, r => (), data = createData(method=POST))
  }
}