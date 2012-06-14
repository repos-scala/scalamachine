package scalamachine.core.tests

import org.specs2._
import scalamachine.core._
import Util._
import org.scalacheck.{Arbitrary, Gen, Prop}
import Prop._
import org.specs2.matcher.Matcher

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
    "If the header value is invalid, an empty list is returned"                     ! { acceptToMediaTypes("abc") must beEmpty } ^
    "If the header value is valid"                                                  ^
      "If the value contains no commas a single element is always returned"         ! singleMediaEntryReturnsSize1 ^
      "If the value contains comma seperated values"                                ^
        "returned list contains entry per value"                                    ! testManyMediaEntries^
        "Sorting"                                                                   ^
          "items are sorted by q val, with default q=1"                             ! testSortedQVals ^
                                                                                    endp^
  "Choosing Content Types given provided types and accept header value"             ^
    "Given an empty list of provided content types, None is returned"               ! testEmptyProvidedValidAccept ^
    "Given an invalid acceptable media header, None is returned"                    ! testInvalidAcceptHeader ^
    "Given a provided list containing one content type and a valid header"          ^
      "If there is no acceptable type None is returned"                             ! testOneProvidedNoAcceptable ^
      "if there is an acceptable content type the only type provided is returned"   ! testOneProvidedWithAcceptable ^p^
    "Given a provided list containing > 1 content type and a valid header"          ^
      "If there is noacceptable type None is returned"                              ! skipped ^
      "If there is an acceptable media type the one w/ highest q value is chosen"   ! skipped ^    
                                                                                    endp^
  "Parsing Accept Charset/Encoding Header Values"                                   ^
    "If the header value is empty, an empty list is returned"                       ! { acceptCharsetToList("") must beEmpty } ^
    "if the header value is valid"                                                  ^
      "If the value contains no commas a single element is always returned"         ! testOneAcceptValue ^
      "If the value contains comma seperated values"                                ^
        "returned list contains entry per value"                                    ! testManyAcceptValues ^
        "Sorting"                                                                   ^
          "items are sorted by q val, with default q=1"                             ! testSortAcceptValues ^
                                                                                    endp^
  "Choosing Charset given provided types and accept-charset header value"           ^
    "Given an empty list of provided content types, None is returned"               ! { chooseAcceptable(Nil, "*", "somedefault") must beNone } ^
    "Given a non-empty provided list and a valid header"                            ^
      "if none of the acceptable types (excluding *) are provided"                  ^
        "if star priority is not 0, first provided type is returned"                ! testNoneAcceptableStarNot0 ^
        "if star priority is 0"                                                     ^
          "if default priority is not 0"                                            ^
            "if it is provided, it is returned"                                     ! testNoneAcceptableDefaultAcceptableProvided ^
            "if it is not provided, none is returned"                               ! testNoneAcceptableDefaultAcceptableNotProvided ^p^
          "if default priority is 0"                                                ^
            "None is returned"                                                      ! testNoneAcceptableDefaultNotAcceptable^p^p^p^
     "if acceptable type contained in provided list"                                ^
       "values with priority 0 are not returned"                                    ! testPriorityZeroNotReturned ^
       "if the priority is not 0"                                                   ^
         "the highest priority value is returned"                                   ! testHighestPriorityReturned ^
                                                                                    end

  // TODO: add tests for sorting by specificity of content type

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
      (fullType + mkQ(qVal) + (if (!params.isEmpty) params.map(s => s + "=" + s).mkString(";",";","") else ""),
        qVal,hasQ)
    } else (fullType,qVal,hasQ)
  }

  val mediaEntry = for {
    (m,_,_) <- mediaEntryAndQ
  } yield m

  def mkQ(qVal: Double): String = ";q=%s" format qVal.toString.take(5)

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

  def testEmptyProvidedValidAccept = chooseMediaType(Nil, "text/plain") must beNone
  def testInvalidAcceptHeader = chooseMediaType(List(ContentType("text/plain")), "invalid") must beNone 

  def testOneProvidedNoAcceptable = {
    val allBeNone: Matcher[Seq[Option[ContentType]]] = beNone.forall
    val testCt = ContentType("text/html")
    val notMatching = List("foo/bar", "application/*", "text/plain")

    (chooseMediaType(testCt :: Nil, notMatching.mkString(", ")) must beNone) and 
      (notMatching.map(chooseMediaType(testCt :: Nil, _)) must allBeNone)
  }
  def testOneProvidedWithAcceptable = {
    val testCt = ContentType("text/html")
    val matching = List("*", "*/*", "text/*", "text/html")

    val allBeExpectedContentType: Matcher[Seq[Option[ContentType]]] = (beSome.which { (_: ContentType) == testCt }).forall

    (matching.map(chooseMediaType(testCt :: Nil, _)) must allBeExpectedContentType) and 
      (chooseMediaType(testCt :: Nil, matching.mkString(", ")) must beSome.like { case ct => ct must beEqualTo(testCt) })
  }

  def testOneAcceptValue = forAll(nonEmptyStr,Gen.choose(0.0,1.0),Arbitrary.arbitrary[Boolean]) {
    (charset: String, qVal: Double, hasQ: Boolean) => {
      val qStr = if (hasQ) mkQ(qVal) else ""
      val string = charset + qStr
      acceptCharsetToList(string) must haveSize(1) and have(
        (tpl: (String,Double)) => {
          tpl._1 == charset && tpl._2 == (if (hasQ) qVal.toString.take(5).toDouble else 1.0)
        }
      )
    }
  }

  def testManyAcceptValues = forAll(
    Gen.containerOf[List,String](nonEmptyStr),
    Gen.containerOf[List,Double](Gen.choose(0.0,1.0)),
    Gen.containerOf[List,Boolean](Arbitrary.arbitrary[Boolean])) {
    (charsets: List[String], qVals: List[Double], hasQs: List[Boolean]) => {
      val zipped = (charsets, qVals, hasQs).zipped.toList
      val string = (for { (c,q,hq) <- zipped } yield { if (hq) c + mkQ(q) else c }).mkString(", ")
      acceptCharsetToList(string) must haveSize(zipped.size)
    }
  }

  def testSortAcceptValues = forAll(
    Gen.containerOf[List,String](nonEmptyStr),
    Gen.containerOf[List,Double](Gen.choose(0.0,1.0)),
    Gen.containerOf[List,Boolean](Arbitrary.arbitrary[Boolean])) {
    (charsets: List[String], qVals: List[Double], hasQs: List[Boolean]) => {
      val zipped = (charsets, qVals, hasQs).zipped.toList
      val string = (for { (c,q,hq) <- zipped } yield { if (hq) c + mkQ(q) else c }).mkString(", ")
      // need to reverse because beSorted checks ascending order and we want descending
      acceptCharsetToList(string).unzip._2.reverse must beSorted
    }
  }.set(minTestsOk -> 10)

  val random = new scala.util.Random
  def testNoneAcceptableStarNot0 = forAll(
    Gen.containerOf[List,String](nonEmptyStr) suchThat { _.size > 0 },
    Gen.containerOf[List,String](nonEmptyStr)  suchThat { _.size > 0 }) {
    (provided: List[String], acceptable: List[String]) => {
      val length = acceptable.length
      val index = if (length == 0) 0 else random.nextInt(length)
      val finalProvided = provided filterNot { acceptable.contains(_) }
      val (beforeStar, afterStar) = acceptable.splitAt(index)
      val headerVal = ((beforeStar :+ "*;q=1.0") ++ afterStar).mkString(", ")
      chooseCharset(finalProvided, headerVal) must beSome.like {
        case str => str must beEqualTo(finalProvided.head)
      }
    }
  }

  def testNoneAcceptableDefaultAcceptableProvided = forAll(
    Gen.containerOf[List,String](nonEmptyStr) suchThat { _.size > 0 },
    Gen.containerOf[List,String](nonEmptyStr)  suchThat { _.size > 0 }) {
      (provided: List[String], acceptable: List[String]) => {
        val default = "ISO-8859-1"
        val finalProvided = default :: (provided filterNot { acceptable.contains(_) })
        val headerVal = acceptable.mkString(", ")
        chooseAcceptable(finalProvided, headerVal, default) must beSome.like {
          case str => str must beEqualTo(default)
        }
      }
    }

  def testNoneAcceptableDefaultAcceptableNotProvided = forAll(
    Gen.containerOf[List,String](nonEmptyStr) suchThat { _.size > 0 },
    Gen.containerOf[List,String](nonEmptyStr)  suchThat { _.size > 0 }) {
    (provided: List[String], acceptable: List[String]) => {
      val finalProvided = (provided filterNot { acceptable.contains(_) })
      val headerVal = acceptable.mkString(", ")
      chooseAcceptable(finalProvided, headerVal, "default") must beNone
    }
  }

  def testNoneAcceptableDefaultNotAcceptable = forAll(
    Gen.containerOf[List,String](nonEmptyStr) suchThat { _.size > 0 },
    Gen.containerOf[List,String](nonEmptyStr)  suchThat { _.size > 0 }) {
    (provided: List[String], acceptable: List[String]) => {
      val default = "ISO-8859-1"
      val finalProvided = default :: (provided filterNot { acceptable.contains(_) })
      val headerVal = acceptable.mkString(", ") + ", " + default + ";q=0"
      chooseAcceptable(finalProvided, headerVal, default) must beNone
    }
  }

  def testPriorityZeroNotReturned = check {
    (list: List[String]) => {
      val headerVal = list.map(_ + ";q=0.0").mkString(", ")
      chooseAcceptable(list, headerVal, "default") must beNone
    }
  }

  def testHighestPriorityReturned = forAll(
    Gen.containerOf[List,String](nonEmptyStr) suchThat { _.size > 0 },
    Gen.containerOf[List,Double](Gen.choose(0.001, 1.0)) suchThat { _.size > 0 }) {
      (list: List[String], qVals: List[Double]) => {
        // nasty hack to make trimmed precision qs unique
        // we need them to be not equal to avoid ambiguity in the test
        // when two have the same priority
        val finalQs = qVals.map(_.toString.take(5)).toSet.toList.map((_: String).toDouble)
        val zipped = (list,finalQs).zipped
        val headerVal = zipped.map(_ + mkQ(_)).mkString(", ")
        val expected = zipped.toList.sortWith(_._2 > _._2).head._1
        chooseAcceptable(zipped.unzip._1.toList, headerVal, "default") must beSome.like {
          case str => str must beEqualTo(expected)
        }
      }
  }
}
