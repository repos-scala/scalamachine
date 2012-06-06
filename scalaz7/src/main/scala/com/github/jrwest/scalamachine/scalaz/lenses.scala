package com.github.jrwest.scalamachine.scalaz

import scalaz.Lens._
import com.github.jrwest.scalamachine.core._

object lenses {

  val baseUriL: ReqRespData @-@ String =
    lensG(_.baseUri, d => u => d copy (baseUri = u))

  val statusCodeL: ReqRespData @-@ Int =
    lensG(_.statusCode, d => c => d copy (statusCode = c))

  val responseHeadersL: ReqRespData @-@ Map[HTTPHeader, String] =
    lensG(_.responseHeaders, d => hdrs => d copy (responseHeaders = hdrs))

  val requestHeadersL: ReqRespData @-@ Map[HTTPHeader, String] =
    lensG(_.requestHeaders, d => hdrs => d copy (requestHeaders = hdrs))

  val methodL: ReqRespData @-@ HTTPMethod =
    lensG(_.method, d => m => d copy (method = m))

  val queryL: ReqRespData @-@ Map[String,List[String]] =
    lensG(_.query, d => q => d copy (query = q))

  val reqBody: ReqRespData @-@ HTTPBody =
    lensG(_.requestBody, d => b => d copy (requestBody = b))

  val respBodyL: ReqRespData @-@ HTTPBody =
    lensG(_.responseBody, d => b => d copy (responseBody = b))

  private val pathDataL: ReqRespData @-@ PathData =
    lensG(_.pathData, d => pd => d copy (pathData = pd))

  val pathInfoL: ReqRespData @-@ Map[Symbol,String] =
    pathDataL <=< lensG(_.info, d => i => d copy (info = i))

  val pathTokensL: ReqRespData @-@ Seq[String] =
    pathDataL <=< lensG(_.tokens, d => ts => d copy (tokens = ts))

  val pathPartsL: ReqRespData @-@ List[String] =
    lensG(_.pathParts, d => p => d copy (pathParts = p))

  val pathL: ReqRespData @-@ String =
    lensG(_.path, d => p => d copy (pathParts = p.split("/").toList))

  val dispPathL: ReqRespData @-@ String =
    lensG(_.dispPath, d => dp => d copy (pathData = d.pathData.copy(tokens = dp.split("/").toList)))

  private val hostDataL: ReqRespData @-@ HostData =
    lensG(_.hostData, d => hd => d copy (hostData = hd))

  val hostInfoL: ReqRespData @-@ Map[Symbol,String] =
    hostDataL <=< lensG(_.info, d => i => d copy (info = i))

  val hostTokensL: ReqRespData @-@ Seq[String] =
    hostDataL <=< lensG(_.tokens, d => ts => d copy (tokens = ts))

  val hostPartsL: ReqRespData @-@ List[String] =
    lensG(_.hostParts, d => ps => d copy (hostParts = ps))

  val hostL: ReqRespData @-@ String =
    lensG(_.host, d => ps => d copy (hostParts = ps.split(".").toList))

  val dispSubdomainL: ReqRespData @-@ String =
    lensG(_.dispSubdomain, d => ds => d copy (hostData = d.hostData.copy(tokens = ds.split(".").toList)))

  val doRedirectL: ReqRespData @-@ Boolean =
    lensG(_.doRedirect, d => b => d copy (doRedirect = b))

}
