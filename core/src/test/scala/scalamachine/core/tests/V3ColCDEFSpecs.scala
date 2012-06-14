package scalamachine.core.tests

import org.specs2._
import mock._
import org.mockito.{Matchers => MM}
import scalamachine.core._
import Resource._
import v3.WebmachineDecisions
import HTTPHeaders._
import HTTPMethods._

class V3ColCDEFSpecs extends Specification with Mockito with SpecsHelper with WebmachineDecisions { def is =
  "Webmachine V3 Column C, D, E & F".title                                          ^
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
        "chosen is set as the value of Content-Encoding header,in meta, G7 returned"! testAcceptEncodingMissingDefaultAcceptable ^p^
      "otherwise, a response with code 406 is returned"                             ! testAcceptEncodingMissingDefaultNotAcceptable ^
                                                                                    p^p^
  "F7 - Accept Encoding Available?"                                                 ^
    "If resource specifies encoding neg. short circuiting, G7 returned"             ! testAcceptEncodingExistsShortCircuit ^
    "If charset is provided by the resource, G7 returned, chosen set in resp./meta" ! testAcceptEncodingExistsAcceptable ^
    "If charset is not provided, response w/ code 406 returned"                     ! testAcceptEncodingExistsNotAcceptable ^
                                                                                    end

  // TODO: change D5 to do real language negotiation like ruby webmachine implementation

  def testMissingAcceptHeader = {
    val ctypes: ContentTypesProvided =
      (ContentType("text/html"), (d: ReqRespData) => (d,(ValueRes(FixedLengthBody("".getBytes))))) ::
        (ContentType("text/plain"), (d: ReqRespData) => ((d, ValueRes(FixedLengthBody("".getBytes))))) ::  Nil

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
    testDecisionReturnsDecision(c3,c4,_.contentTypesProvided(any) answers mkAnswer(Nil),data = createData(headers = Map(Accept -> "text/html")))
  }

  def testMediaTypeNotProvided = {
    val ctypes: ContentTypesProvided = (ContentType("text/html"), (d: ReqRespData) => (d, ValueRes(FixedLengthBody("".getBytes)))) :: Nil

    testDecisionReturnsData(c4,_.contentTypesProvided(any) answers { d => (d.asInstanceOf[ReqRespData], (ValueRes(ctypes))) }, data = createData(headers = Map(Accept -> "text/plain"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testMediaTypeProvided = {
    val ctypes: ContentTypesProvided = (ContentType("text/html"), (d: ReqRespData) => (d, ValueRes(FixedLengthBody("".getBytes)))) :: Nil

    testDecisionReturnsDecisionAndData(c4,d4,_.contentTypesProvided(any) answers mkAnswer(ctypes), data = createData(headers = Map(Accept -> "text/html"))) {
      _.metadata.contentType must beSome.like {
        case ct => ct must beEqualTo(ContentType("text/html"))
      }
    }
  }

  def testMissingAcceptLanguage = {
    testDecisionReturnsDecision(d4,e5,r => {})
  }

  def testHasAcceptLanguage = {
    testDecisionReturnsDecision(d4,d5,r => {},data = createData(headers = Map(AcceptLanguage -> "en/us")))
  }

  def testIsLanguageAvailableFalse = {
    testDecisionReturnsData(d5,_.isLanguageAvailable(any) answers mkAnswer(false), data = createData(headers = Map(AcceptLanguage -> "en/us"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testIsLanguageAvailableTrue = {
    testDecisionReturnsDecision(d5,e5,_.isLanguageAvailable(any) answers mkAnswer(true), data = createData(headers = Map(AcceptLanguage-> "en/us")))
  }

  def testAcceptCharsetExists = {
    testDecisionReturnsDecision(e5,e6,r => {}, data = createData(headers = Map(AcceptCharset -> "*")))
  }

  def testAcceptMissingStarAcceptable = {
    val provided: CharsetsProvided = Some(("abc", identity[Array[Byte]](_)) :: Nil)
    testDecisionReturnsDecision(e5,f6,_.charsetsProvided(any) answers mkAnswer(provided))
  }

  def testAcceptMissingStarOkCharsetChosen = {
    val provided: CharsetsProvided = Some(("abc", identity[Array[Byte]](_)) :: Nil)
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
    testDecisionReturnsDecision(e6, f6, _.charsetsProvided(any) answers mkAnswer(provided), data = createData(headers = Map(AcceptCharset -> "ISO-8859-1")))
  }

  def testAcceptExistsAcceptableSetInMeta = {
    val charset = "ISO-8859-1"
    val provided: CharsetsProvided = Some((charset, identity[Array[Byte]](_)) :: Nil)
    testDecisionReturnsDecisionAndData(e6, f6, _.charsetsProvided(any) answers mkAnswer(provided), data = createData(headers = Map(AcceptCharset -> charset))) {
      _.metadata.chosenCharset must beSome.which { _ == charset }
    }
  }

  def testAcceptExistsNotAcceptable = {
    val provided: CharsetsProvided = Some(Nil)
    testDecisionReturnsData(e6, _.charsetsProvided(any) answers mkAnswer(provided), data = createData(headers = Map(AcceptCharset -> "ISO-8859-1"))) {
      _.statusCode must beEqualTo(406)
    }
  }

  def testF6MediaAndCharsetNotChosen = {
    val provided: EncodingsProvided = None
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided)) {
      _.responseHeader(ContentTypeHeader) must beSome.like {
        case value => value must beEqualTo("text/plain")
      }
    }
  }

  def testF6MediaChosenCharsetNot = {
    val provided: EncodingsProvided = None
    val contentType = ContentType("application/json", Map("a" -> "b", "c" -> "d"))
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided), data = createData(metadata = Metadata(contentType = Some(contentType)))) {
      _.responseHeader(ContentTypeHeader) must beSome.like {
        case value => value must beEqualTo(contentType.mediaType + ";a=b,c=d").ignoreSpace.ignoreCase
      }
    }
  }

  def testF6CharsetChosenMediaNot = {
    val provided: EncodingsProvided = None
    val charset = "ISO-8859-1"
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided), data = createData(metadata = Metadata(chosenCharset = Some(charset)))) {
      _.responseHeader(ContentTypeHeader) must beSome.like {
        case value => value must beEqualTo("text/plain;charset=" + charset).ignoreSpace.ignoreCase
      }
    }
  }

  def testF6MediaAndCharsetChosen = {
    val provided: EncodingsProvided = None
    val contentType = ContentType("application/json", Map("a" -> "b", "c" -> "d"))
    val charset = "ISO-8859-1"
    testDecisionResultHasData(f6, _.encodingsProvided(any) answers mkAnswer(provided), data = createData(metadata = Metadata(contentType = Some(contentType), chosenCharset = Some(charset)))) {
      _.responseHeader(ContentTypeHeader) must beSome.like {
        case value => value must beEqualTo(contentType.mediaType + ";a=b,c=d;charset=" + charset).ignoreSpace.ignoreCase
      }
    }
  }

  def testAcceptEncodingExists = {
    testDecisionReturnsDecision(f6,f7,r => {}, data = createData(headers = Map(AcceptEncoding -> "*")))
  }

  def testAcceptEncodingMissingDefaultAcceptable = {
    val encoding = "some-encoding"
    val provided: EncodingsProvided = Some((encoding, identity[Array[Byte]](_)) :: Nil)
    testDecisionReturnsDecisionAndData(f6,g7,_.encodingsProvided(any) answers mkAnswer(provided)) {
      d => (d.responseHeader(ContentEncoding) must beSome.like {
        case enc => enc must beEqualTo(encoding)
      }) and (d.metadata.chosenEncoding must beSome.like {
        case enc => enc must beEqualTo(encoding)
      })
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
    testDecisionReturnsDecision(f7,g7,_.encodingsProvided(any) answers mkAnswer(provided), data = createData(headers = Map(AcceptEncoding-> "gzip")))
  }

  def testAcceptEncodingExistsAcceptable = {
    val encoding = "gzip"
    val provided: EncodingsProvided = Some((encoding, identity[Array[Byte]](_)) :: Nil)
    testDecisionReturnsDecisionAndData(f7,g7,_.encodingsProvided(any) answers mkAnswer(provided), data = createData(headers = Map(AcceptEncoding -> encoding))) {
      d => (d.responseHeader(ContentEncoding) must beSome.like {
        case enc => enc must beEqualTo(encoding)
      }) and (d.metadata.chosenEncoding must beSome.like {
        case enc => enc must beEqualTo(encoding)
      })
    }
  }

  def testAcceptEncodingExistsNotAcceptable = {
    val provided: EncodingsProvided = Some(Nil)
    testDecisionReturnsData(f7,_.encodingsProvided(any) answers mkAnswer(provided), data = createData(headers = Map(AcceptEncoding -> "ISO-8859-1"))) {
      d => (d.responseHeader(ContentEncoding) must beNone) and (d.statusCode must beEqualTo(406))
    }
  }

}