package scalamachine.core

import scalamachine.internal.scalaz.effect.IO
import scalamachine.internal.scalaz.iteratee.{IterateeT, Input, StepT, EnumeratorT}

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
  import scalamachine.internal.scalaz.Equal
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

  import scalamachine.internal.scalaz.Equal
  import scalamachine.internal.scalaz.Equal._

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
  def bodyType: HTTPBody.Type
  def bytes: Array[Byte]
  def lazyStream: IO[EnumeratorT[HTTPBody.Chunk,IO]]
  lazy val stringValue = this match {
    case FixedLengthBody(bytes) => new String(bytes, java.nio.charset.Charset.forName("UTF-8"))
    case _ => throw new Exception("body is lazy stream. unable to return string value")
  }
  lazy val isEmpty = this match {
    case FixedLengthBody(bytes) => bytes.isEmpty
    case _ => false
  }
  lazy val isStreaming = bodyType match {
    case HTTPBody.FixedLength => false
    case HTTPBody.LazyStream => true
  }
}

object HTTPBody {
  implicit def arrayToHTTPBody(arr: Array[Byte]): HTTPBody = FixedLengthBody(arr)
  implicit def stringToHTTPBody(str: String): HTTPBody = FixedLengthBody(str.getBytes(java.nio.charset.Charset.forName("UTF-8")))

  val Empty: HTTPBody = FixedLengthBody(Array[Byte]())

  sealed trait Chunk
  case class ByteChunk(bytes: Array[Byte]) extends Chunk
  case class ErrorChunk(e: Throwable) extends Chunk
  case object EOFChunk extends Chunk

  sealed trait Type
  case object FixedLength extends Type
  case object LazyStream extends Type
}

case class FixedLengthBody(bytes: Array[Byte]) extends HTTPBody {
  val bodyType = HTTPBody.FixedLength
  def lazyStream: IO[EnumeratorT[HTTPBody.Chunk,IO]] = IO {
    EnumeratorT.enumList[HTTPBody.Chunk,IO](HTTPBody.ByteChunk(bytes) :: HTTPBody.EOFChunk :: Nil)
  }
}
object FixedLengthBody {
  def apply(s: String): HTTPBody = FixedLengthBody(s.getBytes(java.nio.charset.Charset.forName("UTF-8")))
}

object LazyStreamBody {
  def apply(stream: IO[EnumeratorT[HTTPBody.Chunk, IO]]): HTTPBody = new HTTPBody {
    def bytes: Array[Byte] = throw new Exception("body is lazy stream, unable to return all bytes")
    val lazyStream = stream
    val bodyType = HTTPBody.LazyStream
  }

  def apply(produce: () => HTTPBody.Chunk): HTTPBody = {
    val enumerator: EnumeratorT[HTTPBody.Chunk,IO] = new EnumeratorT[HTTPBody.Chunk,IO] {
      lazy val producer: () => HTTPBody.Chunk = produce
      def apply[A] = (s: StepT[HTTPBody.Chunk,IO,A]) =>
        s.mapCont(
         k => {
           IterateeT.iterateeT {
             IO { producer() } except { e => IO(HTTPBody.ErrorChunk(e)) } flatMap {
               _ match {
                 case HTTPBody.EOFChunk => k(Input.elInput(HTTPBody.EOFChunk)).value
                 case e@HTTPBody.ErrorChunk(_) => k(Input.elInput(e)).value
                 case input => (k(Input.elInput(input)) >>== apply[A]).value
               }
             }
           }
         }
        )
    }

    apply(IO(enumerator))
  }

  def apply[I](initialize: => I, produce: I => HTTPBody.Chunk, ensuring: I => Unit = (_: I) => {}): HTTPBody = {
    def enumerator(value: I): EnumeratorT[HTTPBody.Chunk, IO] = new EnumeratorT[HTTPBody.Chunk,IO] {
      lazy val producer: I => HTTPBody.Chunk = produce
      def apply[A] = (s: StepT[HTTPBody.Chunk,IO,A]) =>
        s.mapContOr(
          k => {
            IterateeT.iterateeT {
              IO { producer(value) } except { e => IO(HTTPBody.ErrorChunk(e)) } flatMap { e =>
                (k(Input.elInput(e)) >>== apply[A]).value
              }
            }
          },
          { ensuring(value); s.pointI }
        )
    }

    apply(IO(initialize) map (enumerator(_)) except { e =>
      IO {
        new EnumeratorT[HTTPBody.Chunk, IO] {
          def apply[A] = (s: StepT[HTTPBody.Chunk,IO,A]) =>
            s.mapCont(
             k => k(Input.elInput(HTTPBody.ErrorChunk(e)))
            )
        }
      }
    })
  }

  def forEachChunk(f: HTTPBody.Chunk => Unit): IterateeT[HTTPBody.Chunk, IO, Unit] = IterateeT.fold(())((_, chunk) => f(chunk))
  def forEachChunkUntilFalse(f: HTTPBody.Chunk => Boolean): IterateeT[HTTPBody.Chunk, IO, Unit]  = {
    def step(continue: Boolean): Input[HTTPBody.Chunk] => IterateeT[HTTPBody.Chunk, IO, Unit] = input => input(
      el = e => e match {
        case HTTPBody.EOFChunk =>
          IterateeT.done[HTTPBody.Chunk,IO,Unit](f(e), Input.eofInput)
        case HTTPBody.ErrorChunk(_) =>
          IterateeT.done[HTTPBody.Chunk,IO,Unit](f(e), Input.eofInput)
        case _ =>
          if (continue) IterateeT.cont[HTTPBody.Chunk,IO,Unit](step(f(e))) else IterateeT.done[HTTPBody.Chunk,IO, Unit]((), Input.elInput(e))
      },
      empty = IterateeT.cont[HTTPBody.Chunk,IO,Unit](step(continue)),
      eof = IterateeT.done[HTTPBody.Chunk,IO,Unit]((), Input.eofInput)
    )
    IterateeT.cont(step(true))
  }

  def unapply(body: HTTPBody): Option[IO[EnumeratorT[HTTPBody.Chunk, IO]]] = body.bodyType match {
    case HTTPBody.LazyStream => Some(body.lazyStream)
    case _ => None
  }
}

sealed trait HTTPHeader {
  def lowercaseName: String
  def wireName: String = lowercaseName.split("-").map(_.capitalize).mkString("-")

  override def equals(o: Any) = o match {
    case other: HTTPHeader => other.lowercaseName == lowercaseName
    case _ => false
  }

  override def hashCode = lowercaseName.hashCode

  override def toString = wireName
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
