package com.github.jrwest.scalamachine.core

import com.github.jrwest.scalamachine.internal.scalaz.Lens._
import HTTPMethods._

/**
 * Represents the request being sent by the client as well as the response built by the resource
 *
 * A Note about Headers:
 * framework implementations ensure that only headers that Scalamachine knows
 * about (defined via [[com.github.jrwest.scalamachine.core.HTTPHeaders.createHeader]])
 * are present in header collections. Additionally, header collections do not currenlty support
 * the same header appearing more than once in a request although allowed by the HTTP spec
 * Support for this will be added shortly.
 *
 * Still missing from the original implementation:
 *
 *  - HTTP Version
 *  - Peer (Client) IP
 *  - Raw Path (URI Path including Query String)
 *  - Cookies
 *  - "App Root" - see webmachine documentation (http://wiki.basho.com/Webmachine-Request.html)
 *
 * @param baseUri The base of the requested URI. Includes the scheme and host without the trailing slash (e.g. http://example.com)
 * @param pathParts List of string tokens, the request URI path split by "/"
 * @param method The request's [[com.github.jrwest.scalamachine.core.HTTPMethod]]
 * @param statusCode integer response status code
 * @param query parsed query string. duplicate keys will have multiple elements in the values list
 *              otherwise the values list will only have one element
 * @param requestHeaders Request headers
 * @param responseHeaders Response headers
 * @param requestBody The body of the request. See [[com.github.jrwest.scalamachine.core.HTTPBody]] for more
 * @param responseBody The body of the response to be set by this resource. See [[com.github.jrwest.scalamachine.core.HTTPBody]] for more
 * @param doRedirect If true some responses will return 303 instead of 2xx
 *
 *
 *
 */
case class ReqRespData(baseUri: String = "",
                       pathParts: List[String] = Nil,
                       query: Map[String,List[String]] = Map(),
                       hostParts: List[String] = Nil,
                       method: HTTPMethod = GET,
                       statusCode: Int = 200,
                       requestHeaders: Map[HTTPHeader, String] = Map(),
                       responseHeaders: Map[HTTPHeader, String] = Map(),
                       requestBody: HTTPBody = EmptyBody,
                       responseBody: HTTPBody = EmptyBody,
                       doRedirect: Boolean = false,
                       private[scalamachine] val pathData: PathData = PathData(),
                       private[scalamachine] val hostData: HostData = HostData(),
                       private[core] val metadata: Metadata = Metadata()) {
  

  private[scalamachine] def setPathData(newPathData: PathData) = copy(pathData = newPathData)
  private[scalamachine] def setHostData(newHostData: HostData) = copy(hostData = newHostData)

  val path = pathParts.mkString("/")
  val host = hostParts.mkString(".")

  val pathTokens = pathData.tokens
  val dispPath = pathData.dispPath
  val pathInfo = pathData.info

  val subdomainTokens = hostData.tokens
  val dispSubdomain = hostData.dispSubdomain
  val hostInfo = hostData.info

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
  private[core] val pathL: ReqRespData @-@ String = lensG(_.path, d => p => d copy (pathParts = p.split("/").toList))
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

case class HostData(tokens: Seq[String] = Nil, info: Map[Symbol,String] = Map()) {
  val dispSubdomain = tokens.mkString(".")
}

