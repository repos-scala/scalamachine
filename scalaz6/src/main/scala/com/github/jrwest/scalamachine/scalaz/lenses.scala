package com.github.jrwest.scalamachine.scalaz

import scalaz._
import Scalaz._
import com.github.jrwest.scalamachine.core._

object lenses {

  val baseUriL: Lens[ReqRespData,String] =
    Lens(_.baseUri, (d,u) => d copy (baseUri = u))

  val statusCodeL: Lens[ReqRespData,Int] =
    Lens(_.statusCode, (d, c) => d copy (statusCode = c))

  val responseHeadersL: Lens[ReqRespData,Map[HTTPHeader, String]] =
    Lens(_.responseHeaders, (d, hdrs) => d copy (responseHeaders = hdrs))

  val requestHeadersL: Lens[ReqRespData,Map[HTTPHeader, String]] =
    Lens(_.requestHeaders, (d,hdrs) => d copy (requestHeaders = hdrs))

  val methodL: Lens[ReqRespData,HTTPMethod] =
    Lens(_.method, (d,m) => d copy (method = m))

  val reqBodyL: Lens[ReqRespData, HTTPBody] =
    Lens(_.requestBody, (d,b) => d copy (requestBody = b))

  val respBodyL: Lens[ReqRespData,HTTPBody] =
    Lens(_.responseBody, (d,b) => d copy (responseBody = b))

  private val pathDataL: Lens[ReqRespData,PathData] =
    Lens(_.pathData, (d,pd) => d copy (pathData = pd))

  val pathInfoL: Lens[ReqRespData, Map[Symbol,String]] =
    pathDataL andThen Lens(_.info, (d,i) => d copy (info = i))

  val pathTokensL: Lens[ReqRespData, Seq[String]] =
    pathDataL andThen Lens(_.tokens, (d,ts) => d copy (tokens = ts))

  val pathPartsL: Lens[ReqRespData, List[String]] =
    Lens(_.pathParts, (d,p) => d copy (pathParts = p))

  val pathL: Lens[ReqRespData, String] =
    Lens(_.path, (d,p) => d copy (pathParts = p.split("/").toList))

  val dispPathL: Lens[ReqRespData,String] =
    Lens(_.dispPath, (d,dp) => d copy (pathData = d.pathData.copy(tokens = dp.split("/"))))

  val doRedirectL: Lens[ReqRespData,Boolean] =
    Lens(_.doRedirect, (d,b) => d copy (doRedirect = b))

}
