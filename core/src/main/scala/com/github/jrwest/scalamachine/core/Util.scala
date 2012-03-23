package com.github.jrwest.scalamachine.core

import util.parsing.combinator.JavaTokenParsers


object Util extends AcceptHeaderParsers {
  def acceptToMediaTypes(acceptStr: String): List[MediaInfo] = (parseAll(acceptHeader, acceptStr) getOrElse Nil).sortWith(_.qVal > _.qVal)
}

case class MediaInfo(mediaRangeType: String, mediaRangeParams: List[(String,String)], qVal: Double, acceptParams: List[(String,String)])
case class ContentType(mediaType: String, params: Map[String, String] = Map())

trait AcceptHeaderParsers extends JavaTokenParsers {

  protected def acceptHeader: Parser[List[MediaInfo]] = repsep(mediaInfo, ",")

  protected def mediaInfo: Parser[MediaInfo] = (mediaRange ~ opt(qParam ~ rep(params))) ^^ {
    case range~None => MediaInfo(range._1,range._2,1.0,Nil)
    case range~Some(q~ap) => MediaInfo(range._1,range._2,q,ap)
  }

  protected def qParam: Parser[Double] = ";q=" ~> floatingPointNumber ^^ {
    v => {
      val value = v.toDouble
      if (value < 0) 0.0
      else if (value > 1) 1.0
      else value
    } 
  }

  protected def mediaRange: Parser[(String, List[(String,String)])] = (mediaType ~ rep(params)) ^^ { case head~tail => (head,tail) }

  protected def params: Parser[(String,String)] = ((";" ~> not("q=")) ~> """[^\s=]+""".r) ~ ("=" ~> """[^,;]+""".r) ^^ { case head~tail => (head,tail) }

  protected def mediaType: Parser[String] = """\*/\*""".r | subTypeAll | fullType

  protected def fullType = ((desc <~ "/".r) ~ desc) ^^ { case s~st => s + "/" + st }

  protected def subTypeAll = (desc ~ """/\*""".r) ^^ { case head~tail => head+tail }

  protected def desc: Parser[String] = """[\w\.-]*""".r


}