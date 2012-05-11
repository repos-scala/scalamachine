package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import matcher.MatchResult
import mock._
import org.mockito.{Matchers => MM}
import com.github.jrwest.scalamachine.core._
import Resource._
import flow._
import v3.WebmachineDecisions
import scalaz.NonEmptyList
import NonEmptyList.nel
import scalaz.Digit._0

class WebmachineV3Specs extends Specification with Mockito with WebmachineDecisions { def is = ""            ^ args(sequential=true) ^
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
      "If resource specifies charset negotioation short circuting, F6 is returned"  ! testAcceptMissingCharsetNegShortCircuit ^
      "otherwise, a response with code 406 is returned"                             ! testAcceptMissingStarNotAcceptable ^
                                                                                    p^p^
  "E6 - Accept-Charset Available?"                                                  ^
    "If resource specifies charset negotiation short circuting, F6 is returned"     ! testAcceptExistsCharsetNegShortCircuit ^
    "If the charset is provided by the resource, F6 returned, chosen set in meta"   ! testAcceptExistsAcceptableSetInMeta ^
    "If charset is not provided by the resource, response w/ code 406 returned"     ! testAcceptExistsNotAcceptable ^
                                                                                    p^
  "F6 - Accept-Encoding Exists?"                                                    ^
    "sets the chosen content type/charset in response content type header"          ^
      """if both are None, "text/plain" is set"""                                   ! testF6MediaAndCharsetNotChosen ^
      "if just the content type is Some, its string value is set"                   ! testF6MediaChosenCharsetNot ^
      """if just the charset is Some, "text/plain; charset=<value>" is set"""       ! testF6CharsetChosenMediaNot ^
      "if both are set the entire string is set"                                    ! testF6MediaAndCharsetChosen ^p^
    "if the accept-encoding header exists, decision F7 is returned"                 ! testAcceptEncodingExists ^
    "if the accept-encoding header is missing"                                      ^
      """if "identity;q=1.0,*;q=0.5" is acceptable"""                               ^
        "chosen is set as the value of Content-Encoding header, G7 returned"        ! testAcceptEncodingMissingDefaultAcceptable ^p^
      "otherwise, a response with code 406 is returned"                             ! testAcceptEncodingMissingDefaultNotAcceptable ^
                                                                                    p^p^
  "F7 - Accept Encoding Available?"                                                 ^
    "If resource specifies encoding neg. short circuiting, G7 returned"             ! testAcceptEncodingExistsShortCircuit ^
    "If charset is provided by the resource, G7 returned, chosen set in response"   ! testAcceptEncodingExistsAcceptable ^
    "If charset is not provided, response w/ code 406 returned"                     ! testAcceptEncodingExistsNotAcceptable ^
                                                                                    p^p^
  "G7 - Resource Exists?"                                                           ^
    "Sets the Vary header after conneg"                                             ^
      "vary header contains all values if all 3 headers were used in conneg"        ! testVaryAll ^
      "vary header does not contain accept if list has size 0"                      ! testVaryContentTypesProvided0 ^
      "vary header does not contain accept if list has size 1"                      ! testVaryContentTypesProvided1 ^
      "vary header does not contain accept-charset if short circuited"              ! testVaryCharsetsShortCircuit ^
      "vary header does not contain accept-charset if list has size 0"              ! testVaryCharsetsProvided0 ^
      "vary header does not contain accept-charset if list has size 1"              ! testVaryCharsetsProvided1 ^
      "vary header does not contain accept-encoding if short circuited"             ! testVaryEncodingsShortCircuit ^
      "vary header does not contain accept-encoding if list has size 0"             ! testVaryEncodingsProvided0 ^
      "vary header does not contain accept-encoding if list has size 1"             ! testVaryEncodingsProvided1 ^
      "if resource returns non-empty list, those values are additional"             ! testVaryResourceAdditional ^p^
    "if resource exists, returns decision G8"                                       ! testResourceExistsTrue ^
    "otherwise H7 returned"                                                         ! testResourceExistsFalse ^
                                                                                    p^
  "G8 - If-Match Exists?"                                                           ^
    "if If-Match header exists, G9 is returned"                                     ! testG8IfMatchExists ^
    "otherwise H10 is returned"                                                     ! testG8IfMatchMissing ^
                                                                                    p^
  "G9 - If-Match: *?"                                                               ^
    """if If-Match has value "*", H10 is returned"""                                ! testIfMatchStar ^
    "otherwise G11 is returned"                                                     ! testIfMatchNotStar ^
                                                                                    p^
  "G11 - ETag in If-Match"                                                          ^
    "if ETag for resource is in list of etags in If-Match, H10 is returned"         ! testIfMatchHasEtag ^
    "otherwise a response with code 412 is returned"                                ! testIfMatchersMissingEtag ^
                                                                                    p^
  "H7 - If-Match Exists?"                                                           ^
    "if If-Match header exists, I7 is returned"                                     ! testH7IfMatchExists ^
    "otherwise a response with code 412 is returned"                                ! testH7IfMatchMissing ^
                                                                                    end


  // TODO: tests around halt result, error result, empty result, since that logic is no longer in flow runner where test used to be
  // TODO: change D5 to do real language negotiation like ruby webmachine implementation

  def createResource = mock[Resource]
  def createData(method: HTTPMethod = GET, headers: Map[String,String] = Map(), metadata: Metadata = Metadata()) = ReqRespData(method = method, requestHdrs = headers, metadata = metadata)

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
      _.responseHeader("Allow") must beSome.like {
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
      _.responseHeader("WWW-Authenticate") must beNone
    }
  }
  
  def testAuthFalseErrorResult = {
    testDecisionReturnsData(b8, _.isAuthorized(any) answers mkResAnswer(ErrorRes(null))) {
      _.responseHeader("WWW-Authenticate") must beNone
    }
  }
  
  def testAuthFalseAuthHeader = {
    val headerValue = "somevalue"
    testDecisionReturnsData(b8, _.isAuthorized(any) answers mkAnswer(AuthFailure(headerValue))) {
      _.responseHeader("WWW-Authenticate") must beSome.which { _ == headerValue }
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
    testDecisionReturnsData(b3,_.options(any) answers mkAnswer(Map[String,String]()), data = createData(method=OPTIONS)) {
      _.statusCode must beEqualTo(200)
    }
  }
  
  def testRequestIsOptionsUsesResourceOptionsHeaders = {
    val testHeaders =  Map("X-A" -> "a", "X-B" -> "b")
    testDecisionReturnsData(b3,_.options(any) answers mkAnswer(testHeaders), data = createData(method=OPTIONS)) {
      _.responseHeaders must containAllOf(testHeaders.toList) ^^ { 
        (d1: (String, String), d2: (String, String)) => d1._1.equalsIgnoreCase(d2._1) && d1._2.equalsIgnoreCase(d2._2)
      }
    }
  }

  def testRequestIsNotOptions = {
    testDecisionReturnsDecision(b3,c3, r => (), data = createData(method=POST))
  }

  def testMissingAcceptHeader = {
    val ctypes: ContentTypesProvided =
      (ContentType("text/html"), (d: ReqRespData) => ((ValueRes(""),d))) :: (ContentType("text/plain"), (d: ReqRespData) => ((ValueRes(""), d))) ::  Nil

    testDecisionReturnsDecisionAndData(c3,d4,_.contentTypesProvided(any) answers mkAnswer(ctypes)) {
      _.metadata.contentType must beSome.like {
        case ct => ct must beEqualTo(ctypes.head._1)
      }
    }
  }

  def testMissingAcceptEmptyProvidedList = {
    val ctypes: ContentTypesProvided = Nil

    testDecisionReturnsDecisionAndData(c3,d4,_.contentTypesProvided(any) answers mkAnswer(ctypes)) {
      _.metadata.contentType must beSome.like {
        case ct => ct must beEqualTo(ContentType("text/plain"))
      }
    }    
  }

  def testAcceptHeaderExists = {
    testDecisionReturnsDecision(c3,c4,_.contentTypesProvided(any) answers mkAnswer(Nil),data = createData(headers = Map("accept" -> "text/html")))
  }

  def testMediaTypeNotProvided = {
    val ctypes: ContentTypesProvided = (ContentType("text/html"), (d: ReqRespData) => (ValueRes(""), d)) :: Nil

    testDecisionReturnsData(c4,_.contentTypesProvided(any) answers { d => ((ValueRes(ctypes),d.asInstanceOf[ReqRespData])) }, data = createData(headers = Map("accept" -> "text/plain"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testMediaTypeProvided = {
    val ctypes: ContentTypesProvided = (ContentType("text/html"), (d: ReqRespData) => (ValueRes(""), d)) :: Nil

    testDecisionReturnsDecisionAndData(c4,d4,_.contentTypesProvided(any) answers mkAnswer(ctypes), data = createData(headers = Map("accept" -> "text/html"))) {
      _.metadata.contentType must beSome.like { 
        case ct => ct must beEqualTo(ContentType("text/html"))
      }
    }
  }

  def testMissingAcceptLanguage = {
    testDecisionReturnsDecision(d4,e5,r => {})
  }

  def testHasAcceptLanguage = {
    testDecisionReturnsDecision(d4,d5,r => {},data = createData(headers = Map("accept-language" -> "en/us")))
  }

  def testIsLanguageAvailableFalse = {
    testDecisionReturnsData(d5,_.isLanguageAvailable(any) answers mkAnswer(false), data = createData(headers = Map("accept-language" -> "en/us"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testIsLanguageAvailableTrue = {
    testDecisionReturnsDecision(d5,e5,_.isLanguageAvailable(any) answers mkAnswer(true), data = createData(headers = Map("accept-language" -> "en/us")))
  }

  def testAcceptCharsetExists = {
    testDecisionReturnsDecision(e5,e6,r => {}, data = createData(headers = Map("accept-charset" -> "*")))
  }

  def testAcceptMissingStarAcceptable = {
    val provided: CharsetsProvided = Some(("abc", identity[String](_)) :: Nil)
    testDecisionReturnsDecision(e5,f6,_.charsetsProvided(any) answers mkAnswer(provided))
  }

  def testAcceptMissingStarOkCharsetChosen = {
    val provided: CharsetsProvided = Some(("abc", identity[String](_)) :: Nil)
    testDecisionReturnsDecisionAndData(e5,f6,_.charsetsProvided(any) answers mkAnswer(provided)) {
      _.metadata.chosenCharset must beSome.like { case c => c must beEqualTo("abc") }
    }
  }

  def testAcceptMissingCharsetNegShortCircuit = {
    val provided: CharsetsProvided = None
    testDecisionReturnsDecision(e5,f6,_.charsetsProvided(any) answers mkAnswer(provided))
  }

  def testAcceptMissingStarNotAcceptable = {
    val provided: CharsetsProvided = Some(Nil)
    testDecisionReturnsData(e5,_.charsetsProvided(any) answers mkAnswer(provided)) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testAcceptExistsCharsetNegShortCircuit = {
    val provided: CharsetsProvided = None
    testDecisionReturnsDecision(e6, f6, _.charsetsProvided(any) answers mkAnswer(provided), data = createData(headers = Map("accept-charset" -> "ISO-8859-1")))
  }

  def testAcceptExistsAcceptableSetInMeta = {
    val charset = "ISO-8859-1"
    val provided: CharsetsProvided = Some((charset, identity[String](_)) :: Nil)
    testDecisionReturnsDecisionAndData(e6, f6, _.charsetsProvided(any) answers mkAnswer(provided), data = createData(headers = Map("accept-charset" -> charset))) {
      _.metadata.chosenCharset must beSome.which { _ == charset }
    }
  }

  def testAcceptExistsNotAcceptable = {
    val provided: CharsetsProvided = Some(Nil)
    testDecisionReturnsData(e6, _.charsetsProvided(any) answers mkAnswer(provided), data = createData(headers = Map("acccept-charset" -> "ISO-8859-1"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testF6MediaAndCharsetNotChosen = {
    val provided: EncodingsProvided = None
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided)) {
      _.responseHeader("content-type") must beSome.like {
        case value => value must beEqualTo("text/plain")
      }
    }
  }

  def testF6MediaChosenCharsetNot = {
    val provided: EncodingsProvided = None
    val contentType = ContentType("application/json", Map("a" -> "b", "c" -> "d"))
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided), data = createData(metadata = Metadata(contentType = Some(contentType)))) {
      _.responseHeader("content-type") must beSome.like {
        case value => value must beEqualTo(contentType.mediaType + ";a=b,c=d").ignoreSpace.ignoreCase
      }
    }
  }

  def testF6CharsetChosenMediaNot = {
    val provided: EncodingsProvided = None
    val charset = "ISO-8859-1"
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided), data = createData(metadata = Metadata(chosenCharset = Some(charset)))) {
      _.responseHeader("content-type") must beSome.like {
        case value => value must beEqualTo("text/plain;charset=" + charset).ignoreSpace.ignoreCase
      }
    }
  }

  def testF6MediaAndCharsetChosen = {
    val provided: EncodingsProvided = None
    val contentType = ContentType("application/json", Map("a" -> "b", "c" -> "d"))
    val charset = "ISO-8859-1"
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided), data = createData(metadata = Metadata(contentType = Some(contentType), chosenCharset = Some(charset)))) {
      _.responseHeader("content-type") must beSome.like {
        case value => value must beEqualTo(contentType.mediaType + ";a=b,c=d;charset=" + charset).ignoreSpace.ignoreCase
      }
    }
  }

  def testAcceptEncodingExists = {
    testDecisionReturnsDecision(f6,f7,r => {}, data = createData(headers = Map("accept-encoding" -> "*")))
  }

  def testAcceptEncodingMissingDefaultAcceptable = {
    val encoding = "some-encoding"
    val provided: EncodingsProvided = Some((encoding, identity[String](_)) :: Nil)
    testDecisionReturnsDecisionAndData(f6,g7,_.encodingsProvided(any) answers mkAnswer(provided)) {
      _.responseHeader("content-encoding") must beSome.like {
        case enc => enc must beEqualTo(encoding)
      }
    }
  }

  def testAcceptEncodingMissingDefaultNotAcceptable = {
    val provided: EncodingsProvided = Some(Nil)
    testDecisionReturnsData(f6,_.encodingsProvided(any) answers mkAnswer(provided)) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testAcceptEncodingExistsShortCircuit = {
    val provided: EncodingsProvided = None
    testDecisionReturnsDecision(f7,g7,_.encodingsProvided(any) answers mkAnswer(provided), data = createData(headers = Map("accept-encoding" -> "gzip")))
  }

  def testAcceptEncodingExistsAcceptable = {
    val encoding = "gzip"
    val provided: EncodingsProvided = Some((encoding, identity[String](_)) :: Nil)
    testDecisionReturnsDecisionAndData(f7,g7,_.encodingsProvided(any) answers mkAnswer(provided), data = createData(headers = Map("accept-encoding" -> encoding))) {
      _.responseHeader("content-encoding") must beSome.like {
        case enc => enc must beEqualTo(encoding)
      }
    }
  }

  def testAcceptEncodingExistsNotAcceptable = {
    val provided: EncodingsProvided = Some(Nil)
    testDecisionReturnsData(f7,_.encodingsProvided(any) answers mkAnswer(provided), data = createData(headers = Map("accept-encoding" -> "ISO-8859-1"))) {
      d => (d.responseHeader("content-coding") must beNone) and (d.statusCode must beEqualTo(406))
    }
  }

  def testVaryAll = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must contain("Accept") and contain("Accept-Encoding") and contain("Accept-Charset")
      }
    }
  }

  def testVaryContentTypesProvided0 = {
    import Res._
    val ctypes: ContentTypesProvided = Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not =~("""Accept[^-]""")
      }
    }
  }

  def testVaryContentTypesProvided1 = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not =~("""Accept[^-]""")
      }
    }
  }

  def testVaryCharsetsShortCircuit = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = None
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not contain("Accept-Charset")
      }
    }
  }

  def testVaryCharsetsProvided0 = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(Nil)
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not contain("Accept-Charset")
      }
    }
  }

  def testVaryCharsetsProvided1 = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not contain("Accept-Charset")
      }
    }
  }

  def testVaryEncodingsShortCircuit = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = None
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not contain("Accept-Encoding")
      }
    }

  }

  def testVaryEncodingsProvided0 = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not contain("Accept-Encoding")
      }
    }
  }

  def testVaryEncodingsProvided1 = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must not contain("Accept-Encoding")
      }
    }
  }

  def testVaryResourceAdditional = {
    import Res._
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => (result(""), d)) :: (ContentType("application/json"), (d: ReqRespData) => (result(""), d)) :: Nil
    val charsets: CharsetsProvided = Some(("charset1", identity[String](_)) :: ("charset2", identity[String](_)) :: Nil)
    val encodings: EncodingsProvided = Some(("identity", identity[String](_)) :: ("gzip", identity[String](_)) :: Nil)
    testDecisionResultHasData(
      g7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(ctypes)
        resource.charsetsProvided(any) answers mkAnswer(charsets)
        resource.encodingsProvided(any) answers mkAnswer(encodings)
        resource.variances(any) answers mkAnswer("One" :: "Two" :: Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    ) {
      _.responseHeader("vary") must beSome.like {
        case vary => vary must contain("One") and contain("Two")
      }
    }
  }

  def testResourceExistsTrue = {
    testDecisionReturnsDecision(
      g7,
      g8,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(Nil)
        resource.charsetsProvided(any) answers mkAnswer(None)
        resource.encodingsProvided(any) answers mkAnswer(None)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(true)
      }
    )
  }

  def testResourceExistsFalse = {
    testDecisionReturnsDecision(
      g7,
      h7,
      resource => {
        resource.contentTypesProvided(any) answers mkAnswer(Nil)
        resource.charsetsProvided(any) answers mkAnswer(None)
        resource.encodingsProvided(any) answers mkAnswer(None)
        resource.variances(any) answers mkAnswer(Nil)
        resource.resourceExists(any) answers mkAnswer(false)
      }
    )
  }

  def testG8IfMatchExists = {
    testDecisionReturnsDecision(g8,g9, r => {}, data = createData(headers = Map("if-match" -> "*")))
  }

  def testG8IfMatchMissing = {
    testDecisionReturnsDecision(g8,h10, r => {})
  }

  def testIfMatchStar = {
    testDecisionReturnsDecision(g9,h10, r => {}, data = createData(headers = Map("if-match" -> "*")))
  }

  def testIfMatchNotStar = {
    testDecisionReturnsDecision(g9,g11, r => {}, data = createData(headers = Map("if-match" -> "1")))
  }

  def testIfMatchHasEtag = {
    testDecisionReturnsDecision(g11,h10,_.generateEtag(any) answers mkAnswer(Some("1")), data = createData(headers = Map("if-match" -> "1,2")))
  }

  def testIfMatchersMissingEtag = {
    testDecisionReturnsData(g11,_.generateEtag(any) answers mkAnswer(None), data = createData(headers = Map("if-match" -> "1"))) {
      _.statusCode must beEqualTo(412)
    }
  }

  def testH7IfMatchExists = {
    testDecisionReturnsDecision(h7,i7,r => {}, data = createData(headers = Map("if-match" -> "*")))
  }

  def testH7IfMatchMissing = {
    testDecisionReturnsData(h7,r => {}) {
      _.statusCode must beEqualTo(412)
    }
  }

}
