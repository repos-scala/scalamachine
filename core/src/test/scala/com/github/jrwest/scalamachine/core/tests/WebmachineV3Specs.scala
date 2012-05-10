package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import matcher.MatchResult
import mock._
import com.github.jrwest.scalamachine.core._
import Resource._
import flow._
import v3.WebmachineDecisions
import scalaz.NonEmptyList
import NonEmptyList.nel

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
                                                                                    p^
  "C3 - Accept Exists?"                                                             ^
    "If the Accept header doesn't exist"                                            ^
      "D4 is returned and 1st type in resources provided list is set in metadata"   ! testMissingAcceptHeader ^
      "If provided list empty, text/plain is set in metadata, D4 still returned"    ! testMissingAcceptEmptyProvidedList ^p^
    "If the Accept header exists decision C4 is returned"                           ! testAcceptHeaderExists ^
                                                                                    p^
  "C4 - Acceptable Media Type Available?"                                           ^
    "if the media type is provided by the resource"                                 ^
      "Decision D4 is returned & the mediatype is set as content type in metadata"  ! testMediaTypeProvided ^p^
    "if the media type is not provided by the resource"                             ^
      "response with code 406 is returned"                                          ! testMediaTypeNotProvided ^
                                                                                    p^p^
  "D4 - Accept-Language Exists?"                                                    ^
    "if Accept-Language header exists decision D5 is returned"                      ! testHasAcceptLanguage ^
    "otherwise decision E5 is returned"                                             ! testMissingAcceptLanguage ^
                                                                                    p^
  "D5 - Accept-Language Availble?"                                                  ^
    "asks resource if language is available"                                        ^
      "if it is, decision E5 is returned"                                           ! testIsLanguageAvailableTrue ^
      "otherwise, a response with code 406 is returned"                             ! testIsLanguageAvailableFalse ^
                                                                                    p^p^
  "E5 - Accept-Charset Exists?"                                                     ^
    "If the Accept-Charset header exists decision E6 is returned"                   ! testAcceptCharsetExists ^
    "Otherwise"                                                                     ^
      """If "*" charset is acceptable to resource"""                                ^
        "decision F6 is returned"                                                   ! testAcceptMissingStarAcceptable ^
        "first charset provided by resource is set as chosen in metadata"           ! testAcceptMissingStarOkCharsetChosen ^p^
      "If resource specifies charset negotioation short circuting, F6 is returned"  ! testAcceptMissingCharsetNegShortCurcuit ^
      "otherwise, a response with code 406 is returned"                             ! testAcceptMissingStartNotAcceptable ^
                                                                                    p^p^
  "E6 - Accept-Charset Available?"                                                  ^
    "If resource specifies charset negotiation short circuting, F6 is returned"     ! skipped ^
    "If the charset is provided by the resource"                                    ^
      "the chosen charset is set in the metadata"                                   ! skipped ^
      "decision F6 is returned"                                                     ! skipped ^p^
    "If charset is not provided by the resource, response w/ code 406 returned"     ! skipped ^
                                                                                    end

  // TODO: tests around halt result, error result, empty result, since that logic is no longer in flow runner where test used to be
  // TODO: change D5 to do real language negotiation like ruby webmachine implementation                                                                                    

  def createResource = mock[Resource]
  def createData(method: HTTPMethod = GET, headers: Map[String,String] = Map()) = ReqRespData(method = method, requestHdrs = headers)

  def testDecision(decision: Decision,
                   stubF: (Resource, ReqRespData) => Unit,
                   resource: Resource = createResource,
                   data: ReqRespData = createData())(f: (ReqRespData, Option[Decision]) => MatchResult[Any]): MatchResult[Any] = {
    stubF(resource, data) // make call to stub/mock
    val (mbNextDecision, newData) = decision(resource)(data)
    f(newData, mbNextDecision)
  }

  def testDecisionReturnsDecision(toTest: Decision,
                                  expectedDecision: Decision,
                                  stubF: (Resource, ReqRespData) => Unit,
                                  resource: Resource = createResource,
                                  data: ReqRespData = createData()): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (_: ReqRespData, mbNextDecision: Option[Decision]) => mbNextDecision must beSome.like { case d => d must_== expectedDecision }
    }
  }
  
  def testDecisionReturnsDecisionAndData(toTest: Decision,
                                         expectedDecision: Decision,
                                         stubF: (Resource, ReqRespData) => Unit,
                                         resource: Resource = createResource,
                                         data: ReqRespData = createData())(f: ReqRespData => MatchResult[Any]): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (data: ReqRespData, mbNextDecision: Option[Decision]) => mbNextDecision must beSome.which { _ == expectedDecision } and f(data)
    }
  }
  
  def testDecisionReturnsData(toTest: Decision,
                              stubF: (Resource,ReqRespData) => Unit,
                              resource: Resource = createResource,
                              data: ReqRespData = createData())(f: ReqRespData => MatchResult[Any]): MatchResult[Any] = {
    testDecision(toTest, stubF, resource, data) {
      (retData: ReqRespData, mbNextDecision: Option[Decision]) => (mbNextDecision must beNone) and f(retData)
    }
  }
                          
  
  def testServiceAvailTrue = {
    testDecisionReturnsDecision(b13, b12, (r,d) => r.serviceAvailable(any) returns ((ValueRes(true),d)))
  }

  def testServiceAvailFalse = {
    testDecisionReturnsData(b13, (r,d) => r.serviceAvailable(any) returns ((ValueRes(false), d))) {
      _.statusCode must beEqualTo(503)
    }
  }

  def testKnownMethodTrue = {
    testDecisionReturnsDecision(b12, b11, (r,d) => r.knownMethods(any) returns ((ValueRes(List(GET,POST)), d)))
  }
  
  def testKnownMethodFalse = {
    testDecisionReturnsData(b12, (r,d) => r.knownMethods(any) returns ((ValueRes(List(GET)),d)), data = createData(method = POST)) {
      _.statusCode must beEqualTo(501)
    }
  }
  
  def testURITooLongFalse = {
    testDecisionReturnsDecision(b11, b10, (r,d) => r.uriTooLong(any) returns ((ValueRes(false),d)))
  }
  
  def testURITooLongTrue = {
    testDecisionReturnsData(b11, (r,d) => r.uriTooLong(any) returns ((ValueRes(true),d))) {
      _.statusCode must beEqualTo(414)
    }
  }

  def testAllowedMethodTrue = {
    testDecisionReturnsDecision(b10, b9, (r,d) => r.allowedMethods(any) returns ((ValueRes(List(GET,POST)),d)))
  }

  def testAllowedMethodFalseRespCode = {
    testDecisionReturnsData(b10,(r,d) => r.allowedMethods(any) returns ((ValueRes(List(GET,DELETE)),d)), data = createData(method = POST)) {
      _.statusCode must beEqualTo(405)
    }
  }

  def testAllowedMethodFalseAllowHeader = {
    testDecisionReturnsData(b10, (r, d) => r.allowedMethods(any) returns ((ValueRes(List(GET,POST,DELETE)),d)), data = createData(method=PUT)) {
      _.responseHeader("Allow") must beSome.like {
        case s => s must contain("GET") and contain("POST") and contain("DELETE") // this could be improved (use the actual list above)
      }
    }
  }
  
  def testMalformedFalse = {
    testDecisionReturnsDecision(b9, b8, (r,d) => r.isMalformed(any) returns ((ValueRes(false),d)))
  }

  def testMalformedTrue = {
    testDecisionReturnsData(b9,(r,d) => r.isMalformed(any) returns ((ValueRes(true),d))) {
      _.statusCode must beEqualTo(400)
    }
  }

  def testAuthTrue = {
    testDecisionReturnsDecision(b8, b7, (r,d) => r.isAuthorized(any) returns ((ValueRes(AuthSuccess),d)))
  }
  
  def testAuthFalseRespCode = {
    testDecisionReturnsData(b8,(r,d) => r.isAuthorized(any) returns ((ValueRes(AuthFailure("something")),d))) {
      _.statusCode must beEqualTo(401)
    }
  }

  def testAuthFalseHaltResult = {
    testDecisionReturnsData(b8, (r,d) => r.isAuthorized(any) returns ((HaltRes(500),d))) {
      _.responseHeader("WWW-Authenticate") must beNone
    }
  }
  
  def testAuthFalseErrorResult = {
    testDecisionReturnsData(b8, (r,d) => r.isAuthorized(any) returns ((ErrorRes(null),d))) {
      _.responseHeader("WWW-Authenticate") must beNone
    }
  }
  
  def testAuthFalseAuthHeader = {
    val headerValue = "somevalue"
    testDecisionReturnsData(b8, (r,d) => r.isAuthorized(any) returns ((ValueRes(AuthFailure(headerValue)),d))) {
      _.responseHeader("WWW-Authenticate") must beSome.which { _ == headerValue }
    }
  }
  
  def testForbiddenFalse = {
    testDecisionReturnsDecision(b7,b6,(r,d) => r.isForbidden(any) returns ((ValueRes(false),d)))
  }
  
  def testForbiddenTrue = {
    testDecisionReturnsData(b7,(r,d) => r.isForbidden(any) returns ((ValueRes(true),d))) {
      _.statusCode must beEqualTo(403)
    }
  }

  def testValidContentHeadersTrue = {
    testDecisionReturnsDecision(b6,b5,(r,d) => r.contentHeadersValid(any) returns ((ValueRes(true),d)))
  }
  
  def testValidContentHeadersFalse = {
    testDecisionReturnsData(b6,(r,d) => r.contentHeadersValid(any) returns ((ValueRes(false),d))) {
      _.statusCode must beEqualTo(501)
    }
  }

  def testKnownContentTypeTrue = {
    testDecisionReturnsDecision(b5,b4,(r,d) => r.isKnownContentType(any) returns ((ValueRes(true),d)))
  }
  
  def testKnownContentTypeFalse = {
    testDecisionReturnsData(b5,(r,d) => r.isKnownContentType(any) returns ((ValueRes(false),d))) {
      _.statusCode must beEqualTo(415)
    }
  }
  
  def testIsValidEntityLengthTrue = {
    testDecisionReturnsDecision(b4,b3,(r,d) => r.isValidEntityLength(any) returns ((ValueRes(true),d)))
  }
  
  def testIsValidEntityLengthFalse = {
    testDecisionReturnsData(b4,(r,d) => r.isValidEntityLength(any) returns ((ValueRes(false),d))) {
      _.statusCode must beEqualTo(413)
    }
  }

  def testRequestIsOptions = {
    testDecisionReturnsData(b3,(r,d) => r.options(any) returns ((ValueRes(Map[String,String]()),d)), data = createData(method=OPTIONS)) {
      _.statusCode must beEqualTo(200)
    }
  }
  
  def testRequestIsOptionsUsesResourceOptionsHeaders = {
    val testHeaders =  Map("X-A" -> "a", "X-B" -> "b")
    testDecisionReturnsData(b3,(r,d) => r.options(any) returns ((ValueRes(testHeaders),d)), data = createData(method=OPTIONS)) {
      _.responseHeaders must containAllOf(testHeaders.toList) ^^ { 
        (d1: (String, String), d2: (String, String)) => d1._1.equalsIgnoreCase(d2._1) && d1._2.equalsIgnoreCase(d2._2)
      }
    }
  }

  def testRequestIsNotOptions = {
    testDecisionReturnsDecision(b3,c3, (r,d) => (), data = createData(method=POST))
  }

  def testMissingAcceptHeader = {
    val ctypes: ContentTypesProvided =
      (ContentType("text/html"), (d: ReqRespData) => ((ValueRes(""),d))) :: (ContentType("text/plain"), (d: ReqRespData) => ((ValueRes(""), d))) ::  Nil

    testDecisionReturnsDecisionAndData(c3,d4,(r,d) => r.contentTypesProvided(any) returns ((ValueRes(ctypes),d))) {
      _.metadata.contentType must beSome.like {
        case ct => ct must beEqualTo(ctypes.head._1)
      }
    }
  }

  def testMissingAcceptEmptyProvidedList = {
    val ctypes: ContentTypesProvided = Nil

    testDecisionReturnsDecisionAndData(c3,d4,(r,d) => r.contentTypesProvided(any) returns ((ValueRes(ctypes),d))) {
      _.metadata.contentType must beSome.like {
        case ct => ct must beEqualTo(ContentType("text/plain"))
      }
    }    
  }

  def testAcceptHeaderExists = {
    testDecisionReturnsDecision(c3,c4,(r,d) => r.contentTypesProvided(any) returns ((ValueRes(Nil),d)),data = createData(headers = Map("accept" -> "text/html")))
  }

  def testMediaTypeNotProvided = {
    val ctypes: ContentTypesProvided = (ContentType("text/html"), (d: ReqRespData) => (ValueRes(""), d)) :: Nil

    testDecisionReturnsData(c4,(r,d) => r.contentTypesProvided(any) returns ((ValueRes(ctypes),d)), data = createData(headers = Map("accept" -> "text/plain"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testMediaTypeProvided = {
    val ctypes: ContentTypesProvided = (ContentType("text/html"), (d: ReqRespData) => (ValueRes(""), d)) :: Nil

    testDecisionReturnsDecisionAndData(c4,d4,(r,d) => r.contentTypesProvided(any) returns ((ValueRes(ctypes),d)), data = createData(headers = Map("accept" -> "text/html"))) {
      _.metadata.contentType must beSome.like { 
        case ct => ct must beEqualTo(ContentType("text/html"))
      }
    }
  }

  def testMissingAcceptLanguage = {
    testDecisionReturnsDecision(d4,e5,(r,d) => {})
  }

  def testHasAcceptLanguage = {
    testDecisionReturnsDecision(d4,d5,(r,d) => {},data = createData(headers = Map("accept-language" -> "en/us")))
  }

  def testIsLanguageAvailableFalse = {
    testDecisionReturnsData(d5,(r,d) => r.isLanguageAvailable(any) returns ((ValueRes(false),d)), data = createData(headers = Map("accept-language" -> "en/us"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testIsLanguageAvailableTrue = {
    testDecisionReturnsDecision(d5,e5,(r,d) => r.isLanguageAvailable(any) returns ((ValueRes(true),d)), data = createData(headers = Map("accept-language" -> "en/us")))
  }

  def testAcceptCharsetExists = {
    testDecisionReturnsDecision(e5,e6,(r,d) => {}, data = createData(headers = Map("accept-charset" -> "*")))
  }

  def testAcceptMissingStarAcceptable = {
    val provided: CharsetsProvided = Some(("abc", identity[String](_)) :: Nil)
    testDecisionReturnsDecision(e5,f6,(r,d) => r.charsetsProvided(any) returns ((ValueRes(provided),d)))
  }

  def testAcceptMissingStarOkCharsetChosen = {
    val provided: CharsetsProvided = Some(("abc", identity[String](_)) :: Nil)
    testDecisionReturnsDecisionAndData(e5,f6,(r,d) => r.charsetsProvided(any) returns ((ValueRes(provided),d))) {
      _.metadata.chosenCharset must beSome.like { case c => c must beEqualTo("abc") }
    }
  }

  def testAcceptMissingCharsetNegShortCurcuit = {
    val provided: CharsetsProvided = None
    testDecisionReturnsDecision(e5,f6,(r,d) => r.charsetsProvided(any) returns ((ValueRes(provided), d)))
  }

  def testAcceptMissingStartNotAcceptable = {
    val provided: CharsetsProvided = Some(Nil)
    testDecisionReturnsData(e5,(r,d) => r.charsetsProvided(any) returns ((ValueRes(provided),d))) {
      _.statusCode must beEqualTo(406)
    }
  }
}
