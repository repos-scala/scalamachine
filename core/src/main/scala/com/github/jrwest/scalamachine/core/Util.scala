package com.github.jrwest.scalamachine.core

import util.parsing.combinator.JavaTokenParsers

object Util extends AcceptHeaderParsers {

  def chooseMediaType(provided: List[ContentType], acceptHeader: String) = {

    def isMatch(provided: ContentType, acceptable: ContentType): Boolean = {
      lazy val subStarMatch = {
        (provided.mediaType.split("/").toList, acceptable.mediaType.split("/").toList) match {
          case (p1 :: _ :: Nil, a1 :: "*" :: Nil) if p1 == a1 => true
          case _ => false
        }
      }
      provided == acceptable || subStarMatch
    }

    def tryChoose(provided: List[ContentType], acceptable: MediaInfo): Option[ContentType] = (acceptable, provided) match {
      case (_, Nil) => None
      case (MediaInfo(ContentType("*", mediaParams),_,_), h :: _) if mediaParams.isEmpty => Some(h) // handle lame clients like original webmachine
      case (MediaInfo(ContentType("*/*", mediaParams),_,_), h :: _) if mediaParams.isEmpty => Some(h)
      case (MediaInfo(cType,_,_),_) => provided.find { isMatch(_, cType) }
    }

    def doChoose(provided: List[ContentType], acceptable: List[MediaInfo]): Option[ContentType] = acceptable match {
      case Nil => None
      case h :: rest => tryChoose(provided, h) match {
        case None => doChoose(provided, rest)
        case r @ Some(_) => r
      }
    }

    doChoose(provided, acceptToMediaTypes(acceptHeader))
  }

  val chooseCharset: (List[String], String) => Option[String] = chooseAcceptable(_,_,"ISO-8859-1")
  val chooseEncoding: (List[String], String) => Option[String] = chooseAcceptable(_,_,"identity")

  def chooseAcceptable(provided: List[String], acceptCharsetHeader: String, default: String): Option[String] = {
    @annotation.tailrec
    def doChoose(acceptable: List[(String,Double)]): Option[String] = acceptable match {
      case Nil => None
      case (a, q) :: rest if q > 0 && provided.contains(a) => Some(a)
      case _ :: rest => doChoose(rest)
    }

    lazy val acceptable = acceptCharsetToList(acceptCharsetHeader)
    def tryAnyDefault: Option[String] = {
      val defaultPriority: Option[Double] = acceptable.find(_._1 == default).map(_._2)
      val starPriority: Option[Double] = acceptable.find(_._1 == "*").map(_._2)
      val defaultOk = (starPriority, defaultPriority) match {
        case (Some(q), None) if q == 0.0 => false
        case (Some(_), None) => true
        case (_, Some(q)) if q == 0.0 => false
        case (_, _) => true
      }
      val anyOk = starPriority match {
        case Some(q) if q > 0 => true
        case _ => false
      }

      if (anyOk) provided.headOption
      else if (defaultOk) provided.find(_ == default)
      else None
    }

    doChoose(acceptable) map { Option(_) } getOrElse tryAnyDefault
  }

  def acceptToMediaTypes(acceptStr: String): List[MediaInfo] = (parseAll(acceptHeader, acceptStr) getOrElse Nil).sortWith(_.qVal > _.qVal)

  def acceptCharsetToList(acceptCharsetStr: String): List[(String,Double)] =
    parseAll(acceptCharsets, acceptCharsetStr) getOrElse Nil sortWith(_._2 > _._2)
}


trait AcceptHeaderParsers extends JavaTokenParsers {

  protected def acceptHeader: Parser[List[MediaInfo]] = repsep(mediaInfo, ",")

  protected def mediaInfo: Parser[MediaInfo] = ((mediaRange | crappyMediaRange) ~ opt(qParam ~ rep(params))) ^^ {
    case range~None => MediaInfo(range,1.0,Nil)
    case range~Some(q~ap) => MediaInfo(range,q,ap)
  }

  protected def acceptCharsets: Parser[List[(String,Double)]] = repsep(acceptCharset, """,""".r)

  protected def acceptCharset: Parser[(String,Double)] = """[^,;]+""".r ~ opt(qParam) ^^ { case h~t => (h,t.getOrElse(1.0)) }

  protected def qParam: Parser[Double] = ";q=" ~> floatingPointNumber ^^ {
    v => {
      val value = v.toDouble
      if (value < 0) 0.0
      else if (value > 1) 1.0
      else value
    } 
  }

  protected def crappyMediaRange: Parser[ContentType] = literal("*") ^^^ ContentType("*")

  protected def mediaRange: Parser[ContentType] = (mediaType ~ rep(params)) ^^ { case mtype~args => ContentType(mtype,Map(args:_*)) }

  protected def params: Parser[(String,String)] = ((";" ~> not("q=")) ~> """[^\s=]+""".r) ~ ("=" ~> """[^,;]+""".r) ^^ { case head~tail => (head,tail) }

  protected def mediaType: Parser[String] = """\*/\*""".r | subTypeAll | fullType

  protected def fullType = ((desc <~ "/".r) ~ desc) ^^ { case s~st => s + "/" + st }

  protected def subTypeAll = (desc ~ """/\*""".r) ^^ { case head~tail => head+tail }

  protected def desc: Parser[String] = """[\w\.-]*""".r

}
