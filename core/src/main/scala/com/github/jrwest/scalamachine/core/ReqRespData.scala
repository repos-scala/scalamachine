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
                        requestHeaders: Map[String, String] = Map(),
                        responseHeaders: Map[String, String] = Map(),
                        metadata: Metadata = Metadata()
                        ) {


  // TODO: make private?
  def setPathData(newPathData: PathData) = copy(pathData = newPathData)

  val path = pathParts.mkString("/")
  val pathTokens = pathData.tokens
  val dispPath = pathData.dispPath
  val pathInfo = pathData.info

  def setStatusCode(code: Int) = copy(statusCode = code)

  def requestHeader(name: String) = header(name, requestHeaders)

  def responseHeader(name: String) = header(name, responseHeaders)

  def setResponseHeader(name: String, value: String) = copy(responseHeaders = responseHeaders + (name.toLowerCase -> value))

  def mergeResponseHeaders(newHeaders: Map[String, String]) = copy(responseHeaders = responseHeaders ++ newHeaders)

  private def header(name: String, headers: Map[String, String]) = headers.get(name.toLowerCase)

}