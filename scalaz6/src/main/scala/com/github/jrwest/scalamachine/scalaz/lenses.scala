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
    Lens(_.dispPath, (d,dp) => d copy (pathData = d.pathData.copy(tokens = dp.split("/").toList)))

  private val hostDataL: Lens[ReqRespData,HostData] =
    Lens(_.hostData, (d,hd) => d copy (hostData = hd))

  val hostInfoL: Lens[ReqRespData, Map[Symbol,String]] =
    hostDataL andThen Lens(_.info, (d,i) => d copy (info = i))

  val hostTokensL: Lens[ReqRespData, Seq[String]] =
    hostDataL andThen Lens(_.tokens, (d,ts) => d copy (tokens = ts))

  val hostPartsL: Lens[ReqRespData, List[String]] =
    Lens(_.hostParts, (d,ps) => d copy (hostParts = ps))

  val hostL: Lens[ReqRespData, String] =
    Lens(_.host, (d,ps) => d copy (hostParts = ps.split(".").toList))

  val dispSubdomainL: Lens[ReqRespData, String] =
    Lens(_.dispSubdomain, (d,ds) => d copy (hostData = d.hostData.copy(tokens = ds.split("/").toList)))

  val doRedirectL: Lens[ReqRespData,Boolean] =
    Lens(_.doRedirect, (d,b) => d copy (doRedirect = b))

}
