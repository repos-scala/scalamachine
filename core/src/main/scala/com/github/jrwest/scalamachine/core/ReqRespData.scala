package com.github.jrwest.scalamachine.core

import scalaz.Lens._

// TODO: make headers type alias
// TODO: use scalaz CaseInsensitive for header key?
case class ReqRespData(
                        pathParts: List[String] = Nil,
                        pathData: PathData = PathData(),
                        method: HTTPMethod = GET,
                        statusCode: Int = 200,
                        private val requestHdrs: Map[String, String] = Map(),
                        private val responseHdrs: Map[String, String] = Map(),
                        metadata: Metadata = Metadata()
                        ) {
  
  
  // TODO: make private?
  def setPathData(newPathData: PathData) = copy(pathData = newPathData)

  val path = pathParts.mkString("/")
  val pathTokens = pathData.tokens
  val dispPath = pathData.dispPath
  val pathInfo = pathData.info
  val requestHeaders: Map[String, String] = normalizeHeaders(requestHdrs)
  val responseHeaders: Map[String,String] = normalizeHeaders(responseHdrs)
  
  def setStatusCode(code: Int) = copy(statusCode = code)

  def requestHeader(name: String) = header(name, requestHeaders)

  def responseHeader(name: String) = header(name, responseHeaders)

  def setResponseHeader(name: String, value: String) = copy(responseHdrs = responseHeaders + (name.toLowerCase -> value))

  def mergeResponseHeaders(newHeaders: Map[String, String]) = copy(responseHdrs = responseHeaders ++ newHeaders)

  private def header(name: String, headers: Map[String, String]) = headers.get(name.toLowerCase)

  // TODO: make lazy?
  private def normalizeHeaders(headers: Map[String, String]): Map[String, String] = for { (k,v) <- headers } yield (k.toLowerCase,v)

}

object ReqRespData {
  val statusCodeL: ReqRespData @-@ Int = lensG(_.statusCode, d => c => d copy (statusCode = c))
  val responseHeadersL: ReqRespData @-@ Map[String, String] = lensG(_.responseHeaders, d => hdrs => d copy (responseHdrs = hdrs))
  val requestHeadersL: ReqRespData @-@ Map[String, String] = lensG(_.requestHeaders, d => hdrs => d copy (requestHdrs = hdrs))
  val metadataL: ReqRespData @-@ Metadata = lensG(_.metadata, d => meta => d copy (metadata = meta))
  val methodL: ReqRespData @-@ HTTPMethod = lensG(_.method, d => m => d copy (method = m))
}

case class Metadata(contentType: Option[ContentType] = None, chosenCharset: Option[String] = None)

object Metadata {
  val contentTypeL: Metadata @-@ Option[ContentType] = lensG(_.contentType, m => ct => m copy (contentType = ct))
  val chosenCharsetL: Metadata @-@ Option[String] = lensG(_.chosenCharset, m => cc => m copy (chosenCharset = cc))
}

case class PathData(tokens: Seq[String] = Nil, info: Map[Symbol,String] = Map()) {
  val dispPath = tokens.mkString("/")
}

