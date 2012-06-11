package com.github.jrwest.scalamachine.core

case class MediaInfo(mediaRange: ContentType,
                     qVal: Double,
                     acceptParams: List[(String,String)])

case class ContentType(mediaType: String, params: Map[String, String] = Map()) {
  def toHeader = "%s%s".format(
    mediaType,
    if (params.isEmpty) ""
    else(for { (k,v) <- params } yield k + "=" + v).mkString(";", ",", "") )
}

object ContentType {
  import scalaz.Equal
  import Equal._
  implicit def contentTypeEql: Equal[ContentType] = equalA
}

// not so sure about these yet, was done in a hurry
trait HTTPMethod
object HTTPMethod {
  import HTTPMethods._
  // currently, unknown strings are assumed to be GETS
  // this is under assumption that wherever scalamachine is embedded
  // handles unknown HTTP methods properly, thinking about changing to an unapply
  // and letting implementations decide on how to handle the option (throw/fold/fail/etc)
  def fromString(methodStr: String): HTTPMethod = methodStr.toUpperCase match {
    case "OPTIONS" => OPTIONS
    case "HEAD" => HEAD
    case "TRACE" => TRACE
    case "CONNECT" => CONNECT
    case "DELETE" => DELETE
    case "PUT" => PUT
    case "POST" => POST
    case _ => GET
  }

  import scalaz.Equal
  import scalaz.Equal._

  implicit val httpMethodEqual: Equal[HTTPMethod] = equalA
}

object HTTPMethods {
  case object GET extends HTTPMethod {
    override def toString = "GET"
  }
  case object HEAD extends HTTPMethod {
    override def toString = "HEAD"
  }
  case object POST extends HTTPMethod {
    override def toString = "POST"
  }
  case object PUT extends HTTPMethod {
    override def toString = "PUT"
  }
  case object DELETE extends HTTPMethod {
    override def toString = "DELETE"
  }
  case object TRACE extends HTTPMethod {
    override def toString = "TRACE"
  }
  case object CONNECT extends HTTPMethod {
    override def toString = "CONNECT"
  }
  case object OPTIONS extends HTTPMethod {
    override def toString = "OPTIONS"
  }
}

sealed trait HTTPBody {
  def bytes: Array[Byte]
  val stringValue = new String(bytes, java.nio.charset.Charset.forName("UTF-8"))
  val isEmpty = bytes.isEmpty
}

object HTTPBody {
  implicit def arrayToHTTPBody(arr: Array[Byte]): HTTPBody = FixedLengthBody(arr)
  implicit def stringToHTTPBody(str: String): HTTPBody = FixedLengthBody(str.getBytes(java.nio.charset.Charset.forName("UTF-8")))

  val Empty: HTTPBody = FixedLengthBody(Array[Byte]())
}

case class FixedLengthBody(bytes: Array[Byte]) extends HTTPBody
object FixedLengthBody {
  def apply(s: String): HTTPBody = FixedLengthBody(s.getBytes(java.nio.charset.Charset.forName("UTF-8")))
}

sealed trait HTTPHeader {
  def lowercaseName: String
  def wireName: String = lowercaseName.split("-").map(_.capitalize).mkString("-")

  override def equals(o: Any) = o match {
    case other: HTTPHeader => other.lowercaseName == lowercaseName
    case _ => false
  }

  override def hashCode = lowercaseName.hashCode
}

object HTTPHeader {
  def fromString(s: String): Option[HTTPHeader] = HTTPHeaders.knownHeaders.find(_.lowercaseName == s.toLowerCase)
  def unapply(s: String): Option[HTTPHeader] = fromString(s)
}

object HTTPHeaders {

  private[scalamachine] var knownHeaders: Seq[HTTPHeader] = List()

  def createHeader(name: String): HTTPHeader = {
    val header = new HTTPHeader {
      def lowercaseName: String = name.toLowerCase
    }
    knownHeaders = header +: knownHeaders
    header
  }

  val Accept               = createHeader("accept")
  val AcceptCharset        = createHeader("accept-charset")
  val AcceptEncoding       = createHeader("accept-encoding")
  val AcceptLanguage       = createHeader("accept-language")
  val AcceptRanges         = createHeader("accept-ranges")
  val Age                  = createHeader("age")
  val Allow                = createHeader("allow")
  val CacheControl         = createHeader("cache-control")
  val Connection           = createHeader("connection")
  val ContentDisposition   = createHeader("content-disposition")
  val ContentEncoding      = createHeader("content-encoding")
  val ContentLanguage      = createHeader("content-language")
  val ContentLength        = createHeader("content-length")
  val ContentLocation      = createHeader("content-location")
  val ContentMD5           = createHeader("content-md5")
  val ContentTypeHeader    = createHeader("content-type") // given uncoventional name b/c of existing ContentType class
  val ContentRange         = createHeader("content-range")
  val Cookie               = createHeader("cookie")
  val Date                 = createHeader("date")
  val ETag                 = createHeader("etag")
  val Expect               = createHeader("expect")
  val Expires              = createHeader("expires")
  val From                 = createHeader("from")
  val Host                 = createHeader("host")
  val IfMatch              = createHeader("if-match")
  val IfNoneMatch          = createHeader("if-none-match")
  val IfModifiedSince      = createHeader("if-modified-since")
  val IfRange              = createHeader("if-range")
  val IfUnmodifiedSince    = createHeader("if-unmodified-since")
  val LastModified         = createHeader("last-modified")
  val Link                 = createHeader("link")
  val Location             = createHeader("location")
  val MaxForwards          = createHeader("max-forwards")
  val Pragma               = createHeader("pragma")
  val ProxyAuthenticate    = createHeader("proxy-authenticate")
  val ProxyAuthorization   = createHeader("proxy-authorization")
  val Range                = createHeader("range")
  val Referer              = createHeader("referer")
  val RetryAfter           = createHeader("retry-after")
  val Server               = createHeader("server")
  val TransferEncoding     = createHeader("transfer-encoding")
  val Upgrade              = createHeader("upgrade")
  val UserAgent            = createHeader("user-agent")
  val Vary                 = createHeader("vary")
  val WWWAuthenticate      = createHeader("www-authenticate")
  val XForwardedFor        = createHeader("x-forwarded-for")

}
