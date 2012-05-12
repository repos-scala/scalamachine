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

object HTTPMethod {
  // currently, unknown strings are assumed to be GETS
  // this is under assumption that wherever scalamachine is embedded
  // handles unknown HTTP methods properly, thinking about changing to an unapply
  // and letting implementations decide on how to handle the option (throw/fold/fail/etc)
  def fromString(methodStr: String): HTTPMethod = methodStr.toLowerCase match {
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
