package com.github.jrwest.scalamachine.netty

import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import org.jboss.netty.handler.codec.http._
import com.github.jrwest.scalamachine.core.{HTTPBody, HTTPHeader, HTTPMethod, ReqRespData}
import org.jboss.netty.buffer.ChannelBuffers
import java.net.URI
import scala.collection.JavaConverters._
import scalaz.Id._
import com.github.jrwest.scalamachine.core.v3.V3DispatchTable

trait NettyWebmachine[M[_]] {
  this: DispatchTable[HttpRequest, HttpResponse, M] =>

  // TODO: fill in missing fields
  def toData(req: HttpRequest): ReqRespData = {
    ReqRespData(
      method = HTTPMethod.fromString(req.getMethod.getName),
      pathParts = path(req),
      hostParts = host(
        Option(HttpHeaders.getHost(req))
          .filterNot(_ == "")
          .getOrElse(java.net.InetAddress.getLocalHost.getHostName)
      ),
      baseUri = "http://" + HttpHeaders.getHost(req),
      query = new QueryStringDecoder(req.getUri).getParameters.asScala.mapValues(_.asScala.toList).toMap,
      requestBody = reqBody(req),
      requestHeaders = for {
        (k,v) <- req.getHeaders.asScala.map(entry => (entry.getKey, entry.getValue)).toMap
        hdr <- HTTPHeader.fromString(k)
      }  yield (hdr, v)
    )
  }

  def fromData(data: ReqRespData): HttpResponse = {
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.valueOf(data.statusCode))
    for {
      (h,v) <- data.responseHeaders
    } yield response.setHeader(h.wireName,v)

    response.setContent(ChannelBuffers.copiedBuffer(data.responseBody.bytes)) // TODO: handle streaming body

    response
  }

  private def reqBody(req: HttpRequest): HTTPBody = {
    val contentLength = HttpHeaders.getContentLength(req)
    val array: Array[Byte] = new Array(contentLength.toInt)
    req.getContent.getBytes(0, array)

    array
  }

  private def path(req: HttpRequest): List[String] = {
    val path = new URI(req.getUri).getPath
    val parts = path.split("/").toList
    if (path.startsWith("/")) parts drop 1 else parts
  }

}

trait NettyWebmachineV3 extends V3DispatchTable[HttpRequest, HttpResponse, Id] with NettyWebmachine[Id] {
  def wrap(res: => HttpResponse): Id[HttpResponse] = res
}