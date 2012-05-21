package com.github.jrwest.scalamachine.core

import com.github.jrwest.scalamachine.internal.scalaz.Lens._
import HTTPMethods._

// TODO: make headers type alias
// TODO: use scalaz CaseInsensitive for header key?
case class ReqRespData(
                        baseUri: String = "", // e.g. "http://example.com"
                        pathParts: List[String] = Nil,
                        pathData: PathData = PathData(),
                        method: HTTPMethod = GET,
                        statusCode: Int = 200,
                        requestHeaders: Map[HTTPHeader, String] = Map(),
                        responseHeaders: Map[HTTPHeader, String] = Map(),
                        requestBody: HTTPBody = EmptyBody,
                        responseBody: HTTPBody = EmptyBody,
                        metadata: Metadata = Metadata(),
                        doRedirect: Boolean = false
                        ) {
  

  private[scalamachine] def setPathData(newPathData: PathData) = copy(pathData = newPathData)

  val path = pathParts.mkString("/")
  val pathTokens = pathData.tokens
  val dispPath = pathData.dispPath
  val pathInfo = pathData.info

  def setStatusCode(code: Int) = copy(statusCode = code)

  def requestHeader(header: HTTPHeader) = requestHeaders.get(header)

  def responseHeader(header: HTTPHeader) = responseHeaders.get(header)

  def setResponseHeader(header: HTTPHeader, value: String) = copy(responseHeaders = responseHeaders + (header -> value))

  def mergeResponseHeaders(newHeaders: Map[HTTPHeader, String]) = copy(responseHeaders = responseHeaders ++ newHeaders)

}

object ReqRespData {
  private[core] val baseUriL: ReqRespData @-@ String = lensG(_.baseUri, d => u => d copy (baseUri = u))
  private[core] val statusCodeL: ReqRespData @-@ Int = lensG(_.statusCode, d => c => d copy (statusCode = c))
  private[core] val responseHeadersL: ReqRespData @-@ Map[HTTPHeader, String] = lensG(_.responseHeaders, d => hdrs => d copy (responseHeaders = hdrs))
  private[core] val requestHeadersL: ReqRespData @-@ Map[HTTPHeader, String] = lensG(_.requestHeaders, d => hdrs => d copy (requestHeaders = hdrs))
  private[core] val metadataL: ReqRespData @-@ Metadata = lensG(_.metadata, d => meta => d copy (metadata = meta))
  private[core] val methodL: ReqRespData @-@ HTTPMethod = lensG(_.method, d => m => d copy (method = m))
  private[core] val respBodyL: ReqRespData @-@ HTTPBody = lensG(_.responseBody, d => b => d copy (responseBody = b))
  private[core] val pathDataL: ReqRespData @-@ PathData = lensG(_.pathData, d => pd => d copy (pathData = pd))
  private[core] val dispPathL: ReqRespData @-@ String = lensG(_.dispPath, d => dp => d copy (pathData = d.pathData.copy(tokens = dp.split("/"))))
  private[core] val doRedirectL: ReqRespData @-@ Boolean = lensG(_.doRedirect, d => b => d copy (doRedirect = b))
}

case class Metadata(contentType: Option[ContentType] = None, chosenCharset: Option[String] = None, chosenEncoding: Option[String] = None)

object Metadata {
  private[core] val contentTypeL: Metadata @-@ Option[ContentType] = lensG(_.contentType, m => ct => m copy (contentType = ct))
  private[core] val chosenCharsetL: Metadata @-@ Option[String] = lensG(_.chosenCharset, m => cc => m copy (chosenCharset = cc))
  private[core] val chosenEncodingL: Metadata @-@ Option[String] = lensG(_.chosenEncoding, m => enc => m copy (chosenEncoding = enc))
}

case class PathData(tokens: Seq[String] = Nil, info: Map[Symbol,String] = Map()) {
  val dispPath = tokens.mkString("/")
}

