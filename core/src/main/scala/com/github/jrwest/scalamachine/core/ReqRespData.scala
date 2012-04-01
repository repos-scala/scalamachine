package com.github.jrwest.scalamachine.core

case class Metadata(contentType: Option[ContentType] = None)

case class PathData(tokens: Seq[String] = Nil, info: Map[Symbol,String] = Map()) {
  val dispPath = tokens.mkString("/")
}

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
  import scalaz.Lens._

  val statusCodeL: ReqRespData @-@ Int = lensG(_.statusCode, d => c => d copy (statusCode = c))
  val responseHeadersL: ReqRespData @-@ Map[String, String] = lensG(_.responseHeaders, d => hdrs => d copy (responseHdrs = hdrs))
  

}
