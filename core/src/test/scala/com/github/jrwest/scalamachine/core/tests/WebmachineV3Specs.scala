package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import mock._
import org.mockito.{Matchers => MM}
import com.github.jrwest.scalamachine.core._
import Resource._
import flow._
import v3.WebmachineDecisions
import Res._
import org.apache.commons.httpclient.util.DateUtil
import java.util.Date

class WebmachineV3Specs extends Specification with Mockito with SpecsHelper with WebmachineDecisions { def is = ""            ^
  "WebMachine V3".title                                                             ^
  """
  The WebMachine Version 3 Flow

  http://wiki.basho.com/images/http-headers-status-v3.png
  """                                                                               ^
                                                                                    p^
  "H7 - If-Match Exists?"                                                           ^
    "if If-Match header exists, I7 is returned"                                     ! testH7IfMatchExists ^
    "otherwise a response with code 412 is returned"                                ! testH7IfMatchMissing ^
                                                                                    p^
  "H10 - If-Unmodified-Since Exists?"                                               ^
    "if header exists, H11 is returned"                                             ! testIfUnmodifiedSinceExists ^
    "otherwise I12 is returned"                                                     ! testIfUnmodifiedSinceMissing ^
                                                                                    p^
  "H11 - If-Unmodified-Since Valid Date?"                                           ^
    "if date is valid RFC822/1123, H12 is returned"                                 ! testIUMSRFC822Valid ^
    "if date is valid RFC850 (1036), H12 is returned"                               ! testIUMSRFC850Valid ^
    "if date is valid ANSI C Time, H12 is returned"                                 ! testIUMSANSICValid ^
    "otherwise, I12 is returned"                                                    ! testIUMSInvalid ^
                                                                                    p^
  "H12 - Resource Last Mod. Date > If-Unmodified-Since Date"                        ^
    "if resource's last modified > If-Unmodified-Since, code 412 returned"          ! testIUMSLessThanLastMod ^
    "otherwise, I12 returned"                                                       ! testIUMSGreaterThanLastMod ^
                                                                                    p^
  "I4 - Resource Moved Permanently?"                                                ^
    "checks resource not moved permanently returning P3 if its not"                 ^ testIsResourceMovedPermanently(i4,p3) ^
                                                                                    p^p^
  "I7 - PUT?"                                                                       ^
    "if the HTTP Method is PUT, I4 is returned"                                     ! testIsPutTrue ^
    "otherwise K7 is returned"                                                      ! testIsPutFalse ^
                                                                                    p^
  "I12 - If-None-Match Exists?"                                                     ^
    "if header exists, I13 is returned"                                             ! testIfNoneMatchExists ^
    "otherwise, L13 is returned"                                                    ! testIfNoneMatchMissing ^
                                                                                    p^
  "I13 - If-None-Match: *?"                                                         ^
    """if header has value "*", J18 is returned"""                                  ! testIfNoneMatchStar ^
    "otherwise, K13 is returned"                                                    ! testIfNoneMatchNotStar ^
                                                                                    p^
  "J18 - GET or HEAD?"                                                              ^
    "If request method is GET, response with code 304 is returned"                  ! testJ18IsGet ^
    "If request method is HEAD, response with code 304 is returned"                 ! testJ18IsHead ^
    "otherwise, response with code 412 returned"                                    ! testJ18Neither ^
                                                                                    p^
  "K5 - Resource Moved Peramently?"                                                 ^
    "checks resource not moved permanently returning L5 if its not"                 ^ testIsResourceMovedPermanently(k5,l5) ^
                                                                                    p^p^
  "K7 - Resource Previously Existed"                                                ^
    "if resource returns true, K5 is returned"                                      ! testResourceExistedPrevTrue ^
    "if resource returns false, L7 is returned"                                     ! testResourceExistedPrevFalse ^
                                                                                    p^
  "K13 - ETag in If-None-Match?"                                                    ^
    "if resource's etag is in list of etags, J18 is returned"                       ! testIfNoneMatchHasEtag ^
    "otherwise, L13 is returned"                                                    ! testIfNoneMatchMissingEtag ^
                                                                                    p^
  "L5 - Resource Moved Temporarily?"                                                ^
    "if temp. location returned, loc set in header, code 307 returned"              ! testResourceMovedTemporarily ^
    "otherwise, M5 returned"                                                        ! testResourceNotMovedTemporarily ^
                                                                                    p^
  "L7 - POST?"                                                                      ^ testIsPost(l7,m7,404) ^
                                                                                    p^
  "L13 - If-Modified-Since Exists?"                                                 ^
    "If header exists, L14 is returned"                                             ! testIMSExists ^
    "otherwise, M16 is returned"                                                    ! testIMSMissing ^
                                                                                    p^
  "L14 - If-Modified-Since Valid Date?"                                             ^
    "if date is valid, L15 is returned"                                             ! testIMSValid ^
    "if date is not valid, M15 is returned"                                         ! testIMSInvalid ^
                                                                                    p^
  "L15 - If-Modified-Since > Now?"                                                  ^
    "if date is in the future, M16 is returned"                                     ! testIMSInFuture ^
    "if the date is not in the future, L17 is returned"                             ! testIMSNotInFuture ^
                                                                                    p^
  "L17 - If-Modified-Since > Last-Modified"                                         ^
    "if resource has not been modified since the given time, code 304 is returned"  ! testLastModLessThanIMS ^
    "if resource has been modified since given time, M16 is returned"               ! testLastModGreaterThanIMS ^
                                                                                    p^
  "M5 - POST?"                                                                      ^ testIsPost(m5,n5,410) ^
                                                                                    p^
  "M7 - Can POST to missing resource?"                                              ^
    "if resource returns true, N11 is returned"                                     ! testDecisionReturnsDecision(m7,n11,_.allowMissingPost(any) answers mkAnswer(true)) ^
    "otherwise, response with code 404 is returned"                                 ! testDecisionReturnsData(m7,_.allowMissingPost(any) answers mkAnswer(false)) { _.statusCode must_== 404 } ^
                                                                                    p^
  "M16 - DELETE?"                                                                   ^
    "if request method is DELETE, M20 returned"                                     ! testDecisionReturnsDecision(m16,m20,r => {}, data = createData(method = DELETE)) ^
    "otherwise, N16 returned"                                                       ! testDecisionReturnsDecision(m16,n16,r => {}, data = createData(method = GET)) ^
                                                                                    p^
  "M20 - Call Resource.deleteResource"                                              ^
    "if true is returned, M20b is returned"                                         ! testDecisionReturnsDecision(m20,m20b,_.deleteResource(any) answers mkAnswer(true)) ^
    "if false is returned, response with code 500 is returned"                      ! testDecisionReturnsData(m20,_.deleteResource(any) answers mkAnswer(false)) { _.statusCode must_== 500 } ^
                                                                                    p^
  "M20b - Delete Enacted? (Resource.deleteCompleted)"                               ^
    "if true, O20 is returned"                                                      ! testDecisionReturnsDecision(m20b,o20,_.deleteCompleted(any) answers mkAnswer(true)) ^
    "if false, response with code 202 is returned"                                  ! testDecisionReturnsData(m20b,_.deleteCompleted(any) answers mkAnswer(false)) { _.statusCode must_== 202 } ^
                                                                                    p^
  "N5 - Can POST to missing resource?"                                              ^
    "if true, N11 returned"                                                         ! testDecisionReturnsDecision(n5,n11,_.allowMissingPost(any) answers mkAnswer(true)) ^
    "otherwise, response with code 410 returned"                                    ! testDecisionReturnsData(n5,_.allowMissingPost(any) answers mkAnswer(false)) { _.statusCode must_== 410 } ^
                                                                                    p^
  "N11 - Process Post, Determine Redirect"                                          ^
    "Process Post"                                                                  ^
      "if Resource.postIsCreate returns true"                                       ^
        "Resource.createPath is called"                                             ^
          "if None returned, response with code 500 returned"                       ! testCreatePathNone ^
          "if Some(path) is returned"                                               ^
            "The returned path is set as the dispPath in the ReqRespData"           ! testCreatePathSomeSetsDispPath ^
            "if the location header is not set, the full uri is set as its value"   ! testCreatePathSomeLocationNotSet ^
            "if the location header is set, it is not modified"                     ! testCreatePathSomeLocationAlreadySet ^
            "if request's content-type is not one of those accepted, 415 returned"  ! testN11ContentTypeNotAccepted ^
            "if request's ctype is accepted and corresponding function returns true"^
              "if body is set, it is charsetted then encoded"                       ! testN11ContentTypeAcceptedReturnsTrue ^p^
            "if function corresponing to ctype returns false code 500 returned"     ! testN11ContentTypeAcceptedReturnsFalse ^p^p^
      "if Resource.postIsCreate returns false"                                      ^
        "Resource.processPost is called"                                            ^
          "if true, the body is charsetted then encoded if set"                     ! testProcessPostTrue ^
          "if true and body not set, charsetter and encoder not used"               ! testProcessPostTrueBodyNotSet ^
          "if false, response with code 500 is returned"                            ! testProcessPostFalse ^p^p^p^
    "Determine Redirect"                                                            ^
      "If ReqRespData.doRedirect returns true"                                      ^
        "if Location header is set, response with code 303 returned"                ! testDoRedirect ^
      "If ReqRespData.doRedirect returns false, P11 returned"                       ! testNoRedirect ^
                                                                                    p^p^
  "N16 - POST?"                                                                     ^
    "if request is POST, N11 returned"                                              ! testDecisionReturnsDecision(n16,n11,r => {}, data = createData(method = POST)) ^
    "otherwise, O16 returned"                                                       ! testDecisionReturnsDecision(n16,o16,r => {}, data = createData(method = GET)) ^
                                                                                    p^
  "O14 - Conflict?"                                                                 ^
    "if Resource.isConflict returns true, response w/ code 409 returned"            ! testDecisionReturnsData(o14,_.isConflict(any).answers(mkAnswer(true))) { _.statusCode must beEqualTo(409) } ^
    "otherwise"                                                                     ^
      "if request's ctype is accepted and corresponding func. returns true"         ^
        "if body is set it is charsetted and encoded, P11 returned"                 ! testO14ContentTypeAcceptedReturnsTrue ^p^
      "if request's ctype is accepted but func. returns false, 500 returned"        ! testO14ContentTypeAcceptedReturnsFalse ^
      "if reques's ctype not accepted 415 returned"                                 ! testO14ContentTypeNotAccepted ^
                                                                                    p^p^
  "O16 - PUT?"                                                                      ^
    "if request is PUT, O14 returned"                                               ! testDecisionReturnsDecision(o16,o14,r => {}, data = createData(method = PUT)) ^
    "otherwise, O18 returned"                                                       ! testDecisionReturnsDecision(o16,o18,r => {}, data = createData(method = GET)) ^
                                                                                    p^
  "O18 - Multiple Representations?"                                                 ^
    "If request is a GET or HEAD request"                                           ^
      "if Resource.generateEtag is some, Etag header is set to value"               ! testO18EtagGenerated ^
      "if Resource.lastModified returns date, string value set in Last-Modified"    ! testO18LastModExists ^
      "if Resource.expires returns a datae, string value set in Expires"            ! testO18ExpiresExists ^
      "otherwise Last-Modified, Expires & Etag not set"                             ! testO18NotGenerated  ^
      "chosen content type function is run"                                         ^
        "result is set in body after being charsetted then encoded"                 ! testO18BodyProductionTest ^p^p^
    "If Resource.multipleChoices returns true, response with code 300 returned"     ! testMultipleChoicesTrue ^
    "otherwise response with code 200 returned"                                     ! testMultipleChoicesFalse ^
                                                                                    p^
  "O20 - Response includes an entity?"                                              ^
    "if EmptyBody, response with code 204 returned"                                 ! testDecisionReturnsData(o20,r => {}) { _.statusCode must beEqualTo(204) } ^
    "otherwise, O18 returned"                                                       ! testDecisionReturnsDecision(o20,o18,r =>{},data=createData(respBody="1".getBytes)) ^
                                                                                    end

  // TODO: tests around halt result, error result, empty result, since that logic is no longer in flow runner where test used to be

  def testMultipleChoicesFalse = {
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result("".getBytes), d))) :: Nil
    testDecisionReturnsData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(None)
        r.lastModified(any) answers mkAnswer(None)
        r.expires(any) answers mkAnswer(None)
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(None)
        r.encodingsProvided(any) answers mkAnswer(None)
        r.multipleChoices(any) answers mkAnswer(false)
      },
      data = createData(
        method = GET,
        metadata = Metadata(
          contentType = Some(ContentType("text/plain")),
          chosenEncoding = Some("enc1"),
          chosenCharset = Some("ch1")
        )
      )
    ) { _.statusCode must beEqualTo(200) }
  }

  def testMultipleChoicesTrue = {
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result("".getBytes), d))) :: Nil
    testDecisionReturnsData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(None)
        r.lastModified(any) answers mkAnswer(None)
        r.expires(any) answers mkAnswer(None)
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(None)
        r.encodingsProvided(any) answers mkAnswer(None)
        r.multipleChoices(any) answers mkAnswer(true)
      },
      data = createData(
        method = GET,
        metadata = Metadata(
          contentType = Some(ContentType("text/plain")),
          chosenEncoding = Some("enc1"),
          chosenCharset = Some("ch1")
        )
      )
    ) { _.statusCode must beEqualTo(300) }
  }

  def testO18BodyProductionTest = {
    val setBody: String = "body1"
    val charsetBody: String = "charsetBody"
    val encBody: String = "encbody"
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result(setBody.getBytes), d))) :: Nil
    val charsets: CharsetsProvided = Some(("ch1", ((_: Array[Byte]) ++ charsetBody.getBytes)) :: Nil)
    val encodings: EncodingsProvided = Some(("enc1", ((_: Array[Byte]) ++ encBody.getBytes)) :: Nil)
    testDecisionResultHasData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(None)
        r.lastModified(any) answers mkAnswer(None)
        r.expires(any) answers mkAnswer(None)
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(charsets)
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.multipleChoices(any) answers mkAnswer(false)
      },
      data = createData(
        metadata = Metadata(
          contentType = Some(ContentType("text/plain")),
          chosenEncoding = Some("enc1"),
          chosenCharset = Some("ch1")
        )
      )
    ) {
      _.responseBody.fold(notEmpty = new String(_), empty = "") must beEqualTo(setBody + charsetBody + encBody)
    }
  }

  def testO18ExpiresExists = {
    val expires = new Date(System.currentTimeMillis)
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result("".getBytes), d))) :: Nil
    testDecisionResultHasData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(None)
        r.lastModified(any) answers mkAnswer(None)
        r.expires(any) answers mkAnswer(Some(expires))
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(None)
        r.encodingsProvided(any) answers mkAnswer(None)
        r.multipleChoices(any) answers mkAnswer(false)
      },
      data = createData(
        metadata = Metadata(
          contentType = Some(ContentType("text/plain")),
          chosenEncoding = Some("enc1"),
          chosenCharset = Some("ch1")
        )
      )
    ) {
      _.responseHeader("expires") must beSome.like {
        case date => date must beEqualTo(Util.formatDate(expires))
      }
    }
  }

  def testO18LastModExists = {
    val lastMod = new Date(System.currentTimeMillis)
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result("".getBytes), d))) :: Nil
    testDecisionResultHasData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(None)
        r.lastModified(any) answers mkAnswer(Some(lastMod))
        r.expires(any) answers mkAnswer(None)
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(None)
        r.encodingsProvided(any) answers mkAnswer(None)
        r.multipleChoices(any) answers mkAnswer(false)
      },
      data = createData(
        metadata = Metadata(
          contentType = Some(ContentType("text/plain")),
          chosenEncoding = Some("enc1"),
          chosenCharset = Some("ch1")
        )
      )
    ) {
      _.responseHeader("last-modified") must beSome.like {
        case date => date must beEqualTo(Util.formatDate(lastMod))
      }
    }
  }

  def testO18EtagGenerated = {
    val etag = "etag"
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result("".getBytes), d))) :: Nil
    testDecisionResultHasData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(Some(etag))
        r.lastModified(any) answers mkAnswer(None)
        r.expires(any) answers mkAnswer(None)
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(None)
        r.encodingsProvided(any) answers mkAnswer(None)
        r.multipleChoices(any) answers mkAnswer(false)
      },
      data = createData(method = GET)
    ) {
      _.responseHeader("etag") must beSome.like {
        case e => e must_== etag
      }
    }
  }

  def testO18NotGenerated = {
    val ctypes: ContentTypesProvided = (ContentType("text/plain"), (d: ReqRespData) => ((result("".getBytes), d))) :: Nil
    testDecisionResultHasData(
      o18,
      r => {
        r.generateEtag(any) answers mkAnswer(None)
        r.lastModified(any) answers mkAnswer(None)
        r.expires(any) answers mkAnswer(None)
        r.contentTypesProvided(any) answers mkAnswer(ctypes)
        r.charsetsProvided(any) answers mkAnswer(None)
        r.encodingsProvided(any) answers mkAnswer(None)
        r.multipleChoices(any) answers mkAnswer(false)
      },
      data = createData(method = HEAD)
    ) {
      d =>
        (d.responseHeader("etag") must beNone) and
          (d.responseHeader("last-modified") must beNone) and
          (d.responseHeader("expires") must beNone)
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

  def testIfUnmodifiedSinceExists = {
    testDecisionReturnsDecision(h10, h11, r => {}, data = createData(headers = Map("if-unmodified-since" -> "whocares")))
  }

  def testIfUnmodifiedSinceMissing = {
    testDecisionReturnsDecision(h10,i12,r => {})
  }

  def testIUMSRFC822Valid = {
    testDecisionReturnsDecision(h11,h12,r => {}, data = createData(headers = Map("if-unmodified-since" -> "Sun, 06 Nov 1994 08:49:37 GMT")))
  }

  def testIUMSRFC850Valid = {
    testDecisionReturnsDecision(h11,h12,r => {}, data = createData(headers = Map("if-unmodified-since" -> "Sunday, 06-Nov-94 08:49:37 GMT")))
  }

  def testIUMSANSICValid = {
    testDecisionReturnsDecision(h11,h12,r => {}, data = createData(headers = Map("if-unmodified-since" -> "Sun Nov  6 08:49:37 1994")))
  }

  def testIUMSInvalid = {
    testDecisionReturnsDecision(h11,i12,r => {}, data = createData(headers = Map("if-unmodified-since" -> "invalid")))
  }

  def testIUMSLessThanLastMod = {
    val date = DateUtil.parseDate("Sat, 29 Oct 1995 19:43:31 GMT")
    testDecisionReturnsData(h12,_.lastModified(any) answers mkAnswer(Some(date)), data = createData(headers = Map("if-unmodified-since" -> "Sat, 29 Oct 1994 19:43:31 GMT"))) {
      _.statusCode must beEqualTo(412)
    }
  }

  def testIUMSGreaterThanLastMod = {
    val date = DateUtil.parseDate("Sat, 29 Oct 1993 19:43:31 GMT")
    testDecisionReturnsDecision(h12,i12,_.lastModified(any) answers mkAnswer(Some(date)), data = createData(headers = Map("if-unmodified-since" -> "Sat, 29 Oct 1994 19:43:31 GMT")))
  }

  def testIsPutTrue = {
    testDecisionReturnsDecision(i7,i4, r => {}, data = createData(method = PUT))
  }

  def testIsPutFalse = {
    testDecisionReturnsDecision(i7,k7, r => {})
  }

  def testIfNoneMatchExists = {
    testDecisionReturnsDecision(i12,i13,r => {},data = createData(headers = Map("if-none-match" -> "*")))
  }

  def testIfNoneMatchMissing = {
    testDecisionReturnsDecision(i12,l13,r => {})
  }

  def testIsResourceMovedPermanently(toTest: Decision, proceed: Decision) =
    "if resource returns a location where the resource has been moved"            ^
      "response has Location header set to returned value, and status 301"        ! testResourceMovedPermanently(toTest) ^p^
    "otherwise the decision to proceed with is returned"                          ! testResourceNotMovedPermanently(toTest,proceed)


  def testResourceMovedPermanently(toTest: Decision) = {
    val location = "http://somewhere.com"
    testDecisionReturnsData(toTest,r => r.movedPermanently(any) answers mkAnswer(Some(location))) {
      d => (d.statusCode must beEqualTo(301)) and (d.responseHeader("location") must beSome.like {
        case loc => loc must beEqualTo(location)
      })
    }
  }

  def testResourceNotMovedPermanently(toTest: Decision, proceed: Decision) = {
    testDecisionReturnsDecision(toTest,proceed,r => r.movedPermanently(any) answers mkAnswer(None))
  }

  def testIfNoneMatchStar = {
    testDecisionReturnsDecision(i13,j18,r => {},data = createData(headers = Map("if-none-match" -> "*")))
  }

  def testIfNoneMatchNotStar = {
    testDecisionReturnsDecision(i13,k13,r => {}, data = createData(headers = Map("if-none-match" -> "notstar")))
  }

  def testJ18IsGet = {
    testDecisionReturnsData(j18,r => {}, data = createData(method = GET)) {
      _.statusCode must beEqualTo(304)
    }
  }

  def testJ18IsHead = {
    testDecisionReturnsData(j18,r => {}, data = createData(method = HEAD)) {
      _.statusCode must beEqualTo(304)
    }
  }

  def testJ18Neither = {
    testDecisionReturnsData(j18, r => {}, data = createData(method = POST)) {
      _.statusCode must beEqualTo(412)
    }
  }

  def testResourceExistedPrevTrue = {
    testDecisionReturnsDecision(k7,k5, _.previouslyExisted(any) answers mkAnswer(true))
  }

  def testResourceExistedPrevFalse = {
    testDecisionReturnsDecision(k7,l7, _.previouslyExisted(any) answers mkAnswer(false))
  }

  def testIfNoneMatchHasEtag = {
    testDecisionReturnsDecision(k13,j18,_.generateEtag(any) answers mkAnswer(Some("1")), data = createData(headers = Map("if-none-match" -> "1,2")))
  }

  def testIfNoneMatchMissingEtag = {
    testDecisionReturnsDecision(k13,l13,_.generateEtag(any) answers mkAnswer(None), data = createData(headers = Map("if-none-match" -> "1,2")))
  }

  def testResourceMovedTemporarily = {
    val location = "http://abc.com"
    testDecisionReturnsData(l5,_.movedTemporarily(any) answers mkAnswer(Some(location))) {
      d => (d.statusCode must beEqualTo(307)) and (d.responseHeader("location") must beSome.like {
        case loc => loc must beEqualTo(location)
      })
    }
  }

  def testResourceNotMovedTemporarily = {
    testDecisionReturnsDecision(l5,m5,_.movedTemporarily(any) answers mkAnswer(None))
  }

  def testIsPost(toTest: Decision,whenPost:Decision,whenNot:Int) =
    "if request method is POST, " + whenPost.name + "  is returned"                   ! testRequestIsPost(toTest,whenPost) ^
    "if request method is not POST, response w/ code " + whenNot + " returned"      ! testRequestNotPost(toTest,whenNot)


  def testRequestIsPost(toTest: Decision, expected: Decision) = {
    testDecisionReturnsDecision(toTest,expected,r => {},data = createData(method = POST))
  }

  def testRequestNotPost(toTest: Decision, responseCode: Int) = {
    testDecisionReturnsData(toTest,r => {},data = createData(method = GET)) {
      _.statusCode must beEqualTo(responseCode)
    }
  }

  def testIMSMissing = {
    testDecisionReturnsDecision(l13,m16,r => {})
  }

  def testIMSExists = {
    testDecisionReturnsDecision(l13,l14,r => {}, data = createData(headers = Map("if-modified-since" -> "*")))
  }

  def testIMSValid = {
    testDecisionReturnsDecision(l14,l15,r => {}, data = createData(headers = Map("if-modified-since" -> "Sun, 06 Nov 1994 08:49:37 GMT")))
  }

  def testIMSInvalid = {
    testDecisionReturnsDecision(l14,m16,r => {}, data = createData(headers = Map("if-modified-since" -> "invalid")))
  }

  def testIMSInFuture = {
    // hack
    testDecisionReturnsDecision(l15,m16,r => {}, data = createData(headers = Map("if-modified-since" -> "Sun, 06 Nov 2050 08:49:37 GMT")))
  }

  def testIMSNotInFuture = {
    // hack
    testDecisionReturnsDecision(l15,l17,r => {}, data = createData(headers = Map("if-modified-since" -> "Sun, 06 Nov 1994 08:49:37 GMT")))
  }

  def testLastModGreaterThanIMS = {
    testDecisionReturnsDecision(
      l17,
      m16,
      _.lastModified(any) answers mkAnswer(Util.parseDate("Sun, 06 Nov 1995 08:49:37 GMT")),
      data = createData(headers = Map("if-modified-since" -> "Sun, 06 Nov 1994 08:49:37 GMT"))
    )
  }

  def testLastModLessThanIMS = {
    testDecisionReturnsData(
     l17,
      _.lastModified(any) answers mkAnswer(Util.parseDate("Sun, 06 Nov 1993 08:49:37 GMT")),
      data = createData(headers = Map("if-modified-since" -> "Sun, 06 Nov 1994 08:49:37 GMT"))
    ) {
      _.statusCode must beEqualTo(304)
    }
  }

  def testDoRedirect = {
    val setBody = "body1"
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/plain"), (d: ReqRespData) => (ValueRes(true), d.copy(responseBody = setBody.getBytes))) ::
        (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionReturnsData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some("a/b"))
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.charsetsProvided(any) answers mkAnswer(charsets)
      },
      data = createData(
        metadata = Metadata(chosenCharset = Some("ch1"), chosenEncoding = Some("enc1")),
        headers = Map("content-type" -> "text/plain"),
        respHdrs = Map("location" -> "someloc"),
        doRedirect = true
      )
    ) { _.statusCode must beEqualTo(303) }
  }

  def testNoRedirect = {
    val setBody = "body1"
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/plain"), (d: ReqRespData) => (ValueRes(true), d.copy(responseBody = setBody.getBytes))) ::
        (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionReturnsDecision(
      n11,
      p11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some("a/b"))
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.charsetsProvided(any) answers mkAnswer(charsets)
      },
      data = createData(metadata = Metadata(chosenCharset = Some("ch1"), chosenEncoding = Some("enc1")), headers = Map("content-type" -> "text/plain"))
    )
  }

  def testN11ContentTypeNotAccepted = {
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some("a/b"))
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
      },
      data = createData(headers = Map("content-type" -> "text/html2"))
    ) { _.statusCode must beEqualTo(415) }

  }

  def testN11ContentTypeAcceptedReturnsFalse = {
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some("a/b"))
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
      },
      data = createData(headers = Map("content-type" -> "text/html"))
    ) { _.statusCode must beEqualTo(500) }

  }

  def testN11ContentTypeAcceptedReturnsTrue = {
    val setBody = "body1"
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/plain"), (d: ReqRespData) => (ValueRes(true), d.copy(responseBody = setBody.getBytes))) ::
        (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some("a/b"))
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.charsetsProvided(any) answers mkAnswer(charsets)
      },
      data = createData(metadata = Metadata(chosenCharset = Some("ch1"), chosenEncoding = Some("enc1")), headers = Map("content-type" -> "text/plain"))
    ) {
      _.responseBody.fold(notEmpty = new String(_), empty = "") must beEqualTo(setBody + charsetBody + encodingBody)
    }
  }

  def testCreatePathSomeLocationAlreadySet = {
    val existing = "somelocation"
    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some("a/b"))
        r.contentTypesAccepted(any) answers mkAnswer(Nil)
      },
      data = createData(respHdrs = Map("location" -> existing))
    ) {
      _.responseHeader("location") must beSome.like {
        case loc => loc must beEqualTo(existing)
      }
    }
  }

  def testCreatePathSomeLocationNotSet = {
    val baseUri = "http://example.com/"
    val createPath = "a/v"
    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some(createPath))
        r.contentTypesAccepted(any) answers mkAnswer(Nil)
      },
      data = createData(baseUri = baseUri)
    ) {
      _.responseHeader("location") must beSome.like {
        case loc => loc must beEqualTo("http://example.com/a/v")
      }
    }
  }

  def testCreatePathNone = {
    testDecisionReturnsData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(None)
      }
    ) { _.statusCode must beEqualTo(500) }
  }

  def testCreatePathSomeSetsDispPath = {
    val createPath = "a/b"
    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(true)
        r.createPath(any) answers mkAnswer(Some(createPath))
        r.contentTypesAccepted(any) answers mkAnswer(Nil)
      }
    ) { _.dispPath must beEqualTo(createPath) }
  }

  def testProcessPostTrue = {
    val processPostBody = "body1"
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(false)
        r.processPost(any) answers {
          (d: Any) => {
            val data: ReqRespData = d.asInstanceOf[ReqRespData]
            (ValueRes(true), data.copy(responseBody = processPostBody.getBytes))
          }
        }
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.charsetsProvided(any) answers mkAnswer(charsets)
      },
      data = createData(metadata = Metadata(chosenCharset = Some("ch1"), chosenEncoding = Some("enc1")))
    ) { newData =>
      newData.responseBody.fold(notEmpty = new String(_), empty = "") must beEqualTo(processPostBody + charsetBody + encodingBody)
    }
  }

  def testProcessPostTrueBodyNotSet = {
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(false)
        r.processPost(any) answers mkAnswer(true)
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.charsetsProvided(any) answers mkAnswer(charsets)
      },
      data = createData(metadata = Metadata(chosenCharset = Some("ch1"), chosenEncoding = Some("enc1")))
    ) { _.responseBody must beEqualTo(EmptyBody) }
  }

  def testProcessPostFalse = {
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    testDecisionResultHasData(
      n11,
      r => {
        r.postIsCreate(any) answers mkAnswer(false)
        r.processPost(any) answers mkAnswer(false)
      }
    ) { _.statusCode must beEqualTo(500) }
  }

  def testO14ContentTypeAcceptedReturnsTrue = {
    val setBody = "body1"
    val encodingBody = "body2"
    val charsetBody = "body3"
    val encodings: EncodingsProvided = Some(("enc1", (s: Array[Byte]) => s ++ encodingBody.getBytes) :: ("enc2", identity[Array[Byte]](_)) :: Nil)
    val charsets: EncodingsProvided = Some(("ch1", (s: Array[Byte]) => s ++ charsetBody.getBytes) :: ("ch2", identity[Array[Byte]](_)) :: Nil)
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/plain"), (d: ReqRespData) => (ValueRes(true), d.copy(responseBody = setBody.getBytes))) ::
        (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionReturnsDecisionAndData(
      o14,
      p11,
      r => {
        r.isConflict(any) answers mkAnswer(false)
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
        r.encodingsProvided(any) answers mkAnswer(encodings)
        r.charsetsProvided(any) answers mkAnswer(charsets)
      },
      data = createData(metadata = Metadata(chosenCharset = Some("ch1"), chosenEncoding = Some("enc1")), headers = Map("content-type" -> "text/plain"))
    ) {
      _.responseBody.fold(notEmpty = new String(_), empty = "") must beEqualTo(setBody + charsetBody + encodingBody)
    }
  }

  def testO14ContentTypeAcceptedReturnsFalse = {
    val contentTypesAccepted: ContentTypesAccepted =
      (ContentType("text/html"), (d: ReqRespData) => (ValueRes(false), d)) ::
        Nil

    testDecisionReturnsData(
      o14,
      r => {
        r.isConflict(any) answers mkAnswer(false)
        r.contentTypesAccepted(any) answers mkAnswer(contentTypesAccepted)
      },
      data = createData(headers = Map("content-type" -> "text/html"))
    ) { _.statusCode must beEqualTo(500) }

  }

  def testO14ContentTypeNotAccepted = {
    testDecisionReturnsData(
      o14,
      r => {
        r.isConflict(any) answers mkAnswer(false)
        r.contentTypesAccepted(any) answers mkAnswer(Nil)
      },
      data = createData(headers = Map("content-type" -> "text/html"))
    ) { _.statusCode must beEqualTo(415) }

  }

}
