package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import com.github.jrwest.scalamachine.core.Util._
import org.scalacheck.{Arbitrary, Gen, Prop}
import Prop._

class UtilSpecs extends Specification with ScalaCheck { def is =
  "Utility Functions".title                                                         ^
  """
  This spec contains tests for various utility functions
  that provide functionality like choosing content types
  and languages.
  """                                                                               ^
                                                                                    p^
  "Parsing accept header values"                                                    ^
    "If the header value is empty, an empty list is returned"                       ! { acceptToMediaTypes("") must beEmpty } ^
    "If the header value is invalid, an empty list is returned"                     ! skipped ^
    "If the header value is valid"                                                  ^
      "If the value contains no commas a single element is always returned"         ! singleMediaEntryReturnsSize1 ^
      "If the value contains comma seperated values"                                ^
        "returned list contains entry per value"                                    ! testManyMediaEntries^
        "Sorting"                                                                   ^
          "items are sorted by q val, with default q=1"                             ! testSortedQVals ^
                                                                                    endp^
  "Choosing Content Types given provided types and accept header value"             ^
    "Given an empty list of provided content types, None is returned"               ! skipped ^
    "Given no acceptable media types, None is returned"                             ! skipped ^
    "Given an invalid acceptable media header, None is returned"                    ! skipped ^
    "Given non-empty list provided and valid header"                                ^
      "If there is no acceptable type none is returned"                             ! skipped ^
      "The first acceptable type (from sorted list) is returned otherwise"          ! skipped ^
                                                                                    end

  val nonEmptyStr = Gen.alphaStr suchThat { str =>
    str.size > 0 && str != "q"
  }

  val mediaEntryNoQ = for {
    mediatype <- Gen.frequency((5, nonEmptyStr), (1, "*"))
    subtype <- Gen.frequency((5, nonEmptyStr), (2, "*"))
    nParams <- Gen.choose(0,3) // arbitary choice, seems somewhat practical
    params <- Gen.containerOfN[List,String](nParams,nonEmptyStr)
  } yield {
    if (mediatype == "*") "*/*"
    else {
      mediatype + "/" + subtype  + (if (!params.isEmpty) params.map(s => s + "=" + s).mkString(";",";","") else "")
    }
  } 
  
  val mediaEntryAndQ = for {
    types <- Gen.frequency((5, nonEmptyStr), (1, "*"))
    nParams <- Gen.choose(0,3) // arbitary choice, seems somewhat practical
    params <- Gen.containerOfN[List,String](nParams,nonEmptyStr)
    qVal <- Gen.choose(0.0, 1.0)
    hasQ <- Arbitrary.arbitrary[Boolean]
  } yield {
    val fullType =
      if (types == "*") "*/*"
      else {
        types + "/" + types  + (if (!params.isEmpty) params.map(s => s + "=" + s).mkString(";",";","") else "")
      }
    if (hasQ) {
      (fullType + (";q=%s" format qVal.toString.take(5)) + (if (!params.isEmpty) params.map(s => s + "=" + s).mkString(";",";","") else ""),
        qVal,hasQ)
    } else (fullType,qVal,hasQ)
  }

  val mediaEntry = for {
    (m,_,_) <- mediaEntryAndQ
  } yield m

  // TODO: these could be improved by actually taking the parts of the entry and checking the generated data
  def singleMediaEntryReturnsSize1 = forAll(mediaEntry) {
    (entry: String) => {
      acceptToMediaTypes(entry) must haveSize(1)
    }
  }

  def testManyMediaEntries = forAll(Gen.containerOf[List,String](mediaEntry)) {
    (entries: List[String]) => acceptToMediaTypes(entries.mkString(", ")) must haveSize(entries.size)
  }
  
  def testSortedQVals = forAll(Gen.containerOf[List,(String,Double,Boolean)](mediaEntryAndQ)) {
    (entriesAndQ: List[(String,Double,Boolean)]) => {
      val (entries, qs, hasQs) = entriesAndQ.unzip3
      val realQs = (qs,hasQs).zipped.map((q: Double,h: Boolean) => if (h) q else 1.0)
      val results = acceptToMediaTypes(entries.mkString(", ")).map(_.qVal)
      results must containAllOf(realQs.toList.sortWith(_ > _)).inOrder ^^ ((i: Double, j: Double ) => i-j < 0.01) // adjust for loss of precision

    }
  }.set(minTestsOk->10)

}
