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

  import com.github.jrwest.scalamachine.internal.scalaz.Equal
  import com.github.jrwest.scalamachine.internal.scalaz.Equal._

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

trait HTTPBody {
  def bytes: Array[Byte]
  def isEmpty: Boolean

  def stringValue = new String(bytes)

  def fold[A](notEmpty: Array[Byte] => A, empty: => A): A = this match {
    case EmptyBody => empty
    case NonEmptyBody(bytes) => notEmpty(bytes)
  }

}
case object EmptyBody extends HTTPBody {
  val bytes = Array[Byte]()
  val isEmpty = true
}
case class NonEmptyBody(bytes: Array[Byte]) extends HTTPBody {
  val isEmpty = false
}

object HTTPBody {
  implicit def arrayByteToHTTPBody(a: Array[Byte]): HTTPBody =
    if (a.length > 0 ) NonEmptyBody(a) else EmptyBody
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

  // TODO: all the other standard headers
  val Accept               = createHeader("accept")
  val AcceptCharset        = createHeader("accept-charset")
  val AcceptEncoding       = createHeader("accept-encoding")
  val AcceptLanguage       = createHeader("accept-language")
  val Allow                = createHeader("allow")
  val ContentTypeHeader    = createHeader("content-type") // given uncoventional name b/c of existing ContentType class
  val ContentEncoding      = createHeader("content-encoding")
  val ETag                 = createHeader("etag")
  val Expires              = createHeader("expires")
  val IfMatch              = createHeader("if-match")
  val IfNoneMatch          = createHeader("if-none-match")
  val IfModifiedSince      = createHeader("if-modified-since")
  val IfUnmodifiedSince    = createHeader("if-unmodified-since")
  val LastModified         = createHeader("last-modified")
  val Location             = createHeader("location")
  val Vary                 = createHeader("vary")
  val WWWAuthenticate      = createHeader("www-authenticate")

}
