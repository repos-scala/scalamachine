package com.github.jrwest.scalamachine.core.tests

import org.specs2._
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
    "asks resource if the request method is allowed"                                ^
      "if it is, decision B9 is returned"                                           ! skipped ^
      "if it is not, a response"                                                    ^
        "with code 405 is returned"                                                 ! skipped ^
        "with Allow header set to comma-sep list of allowed methods from resource"  ! skipped ^
                                                                                    p^p^p^
  "B9 - Malformed Request?"                                                         ^
    "asks resource if request is malformed"                                         ^
      "if it is not, decision b8 is returned"                                       ! skipped ^
      "if it is, a response with code 400 is returned"                              ! skipped ^
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

  // TODO: these tests could use some convenience methods to make writing the rest of them faster
  
  def testServiceAvailTrue = {
    val resource = createResource
    val data = createData()
    resource.serviceAvailable(any) returns SimpleResult(true, data)
    val (_, mbNextDecision) = b13.decide(resource, data)
    mbNextDecision must beSome.which { _ == b12 }
  }

  def testServiceAvailFalse = {
    val resource = createResource
    val data = createData()
    resource.serviceAvailable(any) returns SimpleResult(false, data)
    val (retData, mbNextDecision) = b13.decide(resource, data)
    (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(503))
  }

  def testKnownMethodTrue = {
    val resource = createResource
    val data = createData(method = POST)
    resource.knownMethods(any) returns SimpleResult(List(GET,POST), data)
    val (_, mbNextDecision) = b12.decide(resource, data)
    mbNextDecision must beSome.which { _ == b11 } 
  }
  
  def testKnownMethodFalse = {
    val resource = createResource
    val data = createData(method = DELETE)
    resource.knownMethods(any) returns SimpleResult(List(GET), data)
    val (retData, mbNextDecision) = b12.decide(resource, data)
    (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(501))
  }
  
  def testURITooLongFalse = {
    val resource = createResource
    val data = createData()
    resource.uriTooLong(any) returns SimpleResult(false, data)
    val (_, mbNextDecision) = b11.decide(resource, data)
    mbNextDecision must beSome.which { _ == b10 }
  }
  
  def testURITooLongTrue = {
    val resource = createResource
    val data = createData()    
    resource.uriTooLong(any) returns SimpleResult(true, data)
    val (retData, mbNextDecision) = b11.decide(resource, data)
    (mbNextDecision must beNone) and (retData.statusCode must beEqualTo(414))
  }
  
}