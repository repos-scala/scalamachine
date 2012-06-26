package scalamachine.scalaz

import scalaz.LensT._
import scalaz.@>
import scalamachine.core._

object lenses {

  val baseUriL: ReqRespData @> String =
    lensg(d => u => d copy (baseUri = u), _.baseUri)

  val rawPathL: ReqRespData @> String =
    lensg(d => p => d copy (rawPath = p), _.rawPath)

  val statusCodeL: ReqRespData @> Int =
    lensg(d => c => d copy (statusCode = c), _.statusCode)

  val responseHeadersL: ReqRespData @> Map[HTTPHeader, String] =
    lensg(d => hdrs => d copy (responseHeaders = hdrs), _.responseHeaders)

  val requestHeadersL: ReqRespData @> Map[HTTPHeader, String] =
    lensg(d => hdrs => d copy (requestHeaders = hdrs), _.requestHeaders)

  val methodL: ReqRespData @> HTTPMethod =
    lensg(d => m => d copy (method = m), _.method)

  val queryL: ReqRespData @> Map[String,List[String]] =
    lensg(d => q => d copy (query = q), _.query)

  val reqBody: ReqRespData @> HTTPBody =
    lensg(d => b => d copy (requestBody = b), _.requestBody)

  val respBodyL: ReqRespData @> HTTPBody =
    lensg(d => b => d copy (responseBody = b), _.responseBody)

  private val pathDataL: ReqRespData @> PathData =
    lensg(d => pd => d copy (pathData = pd), _.pathData)

  val pathInfoL: ReqRespData @> Map[Symbol,String] =
    pathDataL >=> lensg(d => i => d copy (info = i), _.info)

  val pathTokensL: ReqRespData @> Seq[String] =
    pathDataL >=> lensg(d => ts => d copy (tokens = ts), _.tokens)

  val pathPartsL: ReqRespData @> List[String] =
    lensg(d => p => d copy (pathParts = p), _.pathParts)

  val pathL: ReqRespData @> String =
    lensg(d => p => d copy (pathParts = p.split("/").toList), _.path)

  val dispPathL: ReqRespData @> String =
    lensg(d => dp => d copy (pathData = d.pathData.copy(tokens = dp.split("/").toList)), _.dispPath)

  private val hostDataL: ReqRespData @> HostData =
    lensg(d => hd => d copy (hostData = hd), _.hostData)

  val hostInfoL: ReqRespData @> Map[Symbol,String] =
    hostDataL >=> lensg(d => i => d copy (info = i), _.info)

  val hostTokensL: ReqRespData @> Seq[String] =
    hostDataL >=> lensg(d => ts => d copy (tokens = ts), _.tokens)

  val hostPartsL: ReqRespData @> List[String] =
    lensg(d => ps => d copy (hostParts = ps), _.hostParts)

  val hostL: ReqRespData @> String =
    lensg(d => ps => d copy (hostParts = ps.split(".").toList), _.host)

  val dispSubdomainL: ReqRespData @> String =
    lensg(d => ds => d copy (hostData = d.hostData.copy(tokens = ds.split(".").toList)), _.dispSubdomain)

  val doRedirectL: ReqRespData @> Boolean =
    lensg(d => b => d copy (doRedirect = b), _.doRedirect)

}
