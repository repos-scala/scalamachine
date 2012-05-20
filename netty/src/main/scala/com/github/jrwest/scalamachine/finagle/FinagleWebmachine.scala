package com.github.jrwest.scalamachine.finagle

import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import com.twitter.util.Future
import java.net.URI
import scala.collection.JavaConverters._
import org.jboss.netty.buffer.ChannelBuffers
import com.github.jrwest.scalamachine.core.v3.V3DispatchTable
import org.jboss.netty.handler.codec.http._
import com.github.jrwest.scalamachine.core.{HTTPHeader, HTTPMethod, ReqRespData}

trait FinagleWebmachine {
  this: DispatchTable[HttpRequest, HttpResponse, Future] =>

  def path(req: HttpRequest): List[String] = {
    val path = new URI(req.getUri).getPath
    val parts = path.split("/").toList
    if (path.startsWith("/")) parts drop 1 else parts
  }

  // TODO: fill in base URI and other fields
  def toData(req: HttpRequest): ReqRespData =
    ReqRespData(
      method = HTTPMethod.fromString(req.getMethod.getName),
      pathParts = path(req),
      baseUri = "http://" + req.getHeader(HttpHeaders.Names.HOST),
      requestHeaders = for {
        (k,v) <- req.getHeaders.asScala.map(entry => (entry.getKey, entry.getValue)).toMap
        hdr <- HTTPHeader.fromString(k)
      }  yield (hdr, v)
    )

  def fromData(data: ReqRespData): HttpResponse = {
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.valueOf(data.statusCode))
    for {
      (h,v) <- data.responseHeaders
    } yield response.setHeader(h.wireName,v)

    response.setContent(ChannelBuffers.copiedBuffer(
      data.responseBody.fold(notEmpty = identity(_), empty = Array[Byte]())
    ))

    response
  }

  def wrap(res: => HttpResponse) = Future(res)

}

trait FinagleWebmachineV3 extends V3DispatchTable[HttpRequest,HttpResponse,Future] with FinagleWebmachine