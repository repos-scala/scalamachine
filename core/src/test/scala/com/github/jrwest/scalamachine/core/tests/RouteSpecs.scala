package com.github.jrwest.scalamachine.core.tests

import org.specs2._
import com.github.jrwest.scalamachine.core.Route._
import org.scalacheck.{Arbitrary, Prop, Gen}
import Prop._
import com.github.jrwest.scalamachine.core._


class RouteSpecs extends Specification with ScalaCheck { def is =
  "Routes".title                                                                    ^
  """
  Routes are a partial function from a request path
  to a resource
  """                                                                               ^
                                                                                    p^
  "Matching Paths"                                                                  ^
    "Routes with no data parts"                                                     ^
      "if the route does not end in a star term"                                    ^
        "path matches iif it each token is equal to each string part"               ! AllStringRoutesNoStar.testExactMatchingPath ^
        "path does not match if >= one token is not equal to a string part"         ! AllStringRoutesNoStar.testPartDoesntMatch ^
        "path does not match if it has less tokens than route parts"                ! AllStringRoutesNoStar.testLessTokensThanParts ^
        "path does not match if it has more tokens than route parts"                ! AllStringRoutesNoStar.testMoreTokensThanParts ^
        "path data always has as empty dispatch path string"                        ! AllStringRoutesNoStar.testDispPathAlwaysEmpty ^
        "path data always has an empty path tokens Seq"                             ! AllStringRoutesNoStar.testPathTokensAlwaysEmpty ^p^
      "if the route does end in a star term"                                        ^
        "path matches if each token is equal to each string part"                   ! AllStringRoutesWithStar.testExactStartingWithMatch ^
        "path matches with tokens equal to parts even with leftover tokens"         ! AllStringRoutesWithStar.testLeftoverTokensStillMatch ^
        "path does not match if >= one token is not equal to a string part"         ! AllStringRoutesWithStar.testPartDoesntMatch ^
        "path does not match if there are less tokens than route parts"             ! AllStringRoutesWithStar.testLessTokensThanParts ^
        "path data has an empty dispatch string if num tokens == num route terms"   ! AllStringRoutesWithStar.testDispPathAlwaysEmpty ^
        "path data has left over tokens seperated by slash as dispatch string"      ! AllStringRoutesWithStar.testLeftoverTokensDispPath ^
        "path data has empty path tokens Seq if num tokens == num route terms"      ! AllStringRoutesWithStar.testLeftoverTokensPathTokens ^
        "path data has left over tokens in path tokens Seq with order preserved"    ! AllStringRoutesWithStar.testPathTokensAlwaysEmpty ^p^
      "path data always have empty path info map"                                   ! testStringRoutesAlwaysHaveEmptyPathInfo ^p^
    "Routes with data parts"                                                        ^
      "if the route does not end in a star term"                                    ^
        "if the route contains string terms"                                        ^
          "path matches iif each token that has corresponding string part matches"  ! MixedRoutesNoStar.testMatchesIIFStringPartsMatch ^
          "path does not match if >= one token does not match corresponding string" ! MixedRoutesNoStar.testStringPartDoesntMatch ^
          "path does not match if it has less tokens than route parts"              ! MixedRoutesNoStar.testLessTokensThanParts ^
          "path does not match if has more tokens than route parts"                 ! MixedRoutesNoStar.testMoreTokensThanParts ^
          "path data always has as empty dispatch path string"                      ! MixedRoutesNoStar.testDispPathAlwaysEmpty ^
          "path data always has an empty path tokens Seq"                           ! MixedRoutesNoStar.testPathTokensAlwaysEmpty ^p^
        "if the route does not contain string terms"                                ^
          "path matches only if the number of tokens equal the number of terms"     ! AllDataRouteNoStar.testEqualLengths ^
          "the path does not match otherwise"                                       ! AllDataRouteNoStar.testUnequalLengths ^
          "path data always has as empty dispatch path string"                      ! AllDataRouteNoStar.testDispPathAlwaysEmpty ^
          "path data always has an empty path tokens Seq"                           ! AllDataRouteNoStar.testPathTokensAlwaysEmpty ^p^p^
      "if the route ends in a star term"                                            ^
        "if the route contains string terms"                                        ^
          "path matches if each token thas has corresponding string part matches"   ! MixedRoutesWithStar.testMatchesIIFStringPartsMatch ^
          "path matches with tokens equal to string parts even with leftover tokens"! MixedRoutesWithStar.testMatchesWithLeftoverTokens ^
          "path does not match if >= one token is not equal to corresponding string"! MixedRoutesWithStar.testStringPartDoesntMatch ^
          "path does not match if there are less tokens than route parts"           ! MixedRoutesWithStar.testLessTokensThanParts ^
          "path data has an empty dispatch string if num tokens == num route terms" ! MixedRoutesWithStar.testDispPathAlwaysEmpty ^
          "path data has left over tokens seperated by slash as dispatch string"    ! MixedRoutesWithStar.testLeftoverTokensDispPath ^
          "path data has empty path tokens Seq if num tokens == num route terms"    ! MixedRoutesWithStar.testPathTokensAlwaysEmpty  ^
          "path data has left over tokens in path tokens Seq with order preserved"  ! MixedRoutesWithStar.testLeftoverTokensPathTokens ^p^
        "if the route does not contain string terms"                                ^
          "path matches only if the number of tokens is >= number of terms"         ! AllDataRouteWithStar.testWithLeftoverTokens ^
          "the path does not match otherwise"                                       ! AllDataRouteWithStar.testLessTokensThanParts ^
          "path data has an empty dispatch string if num tokens == num route terms" ! AllDataRouteWithStar.testDispPathAlwaysEmpty ^
          "path data has left over tokens seperated by slash as dispatch string"    ! AllDataRouteWithStar.testLeftoverTokensDispPath ^
          "path data has empty path tokens Seq if num tokens == num route terms"    ! AllDataRouteWithStar.testPathTokensAlwaysEmpty ^
          "path data has left over tokens in path tokens Seq with order preserved"  ! AllDataRouteWithStar.testLeftoverTokensPathTokens ^p^p^
      "path data's path info contains key corresponding to token for each data part"! testMixedRoutesPathInfo ^
                                                                                    end



    
  trait PathDataShared {
    def routeF: List[RouteTerm] => Route
    def toPart: String => RoutePart

    def testDispPathAlwaysEmpty = check {
      (pathParts: List[String]) => routeF(pathParts.map(toPart(_))).apply(pathParts)._2.dispPath must beEmpty
    }

    def testPathTokensAlwaysEmpty = check {
      (pathParts: List[String]) => routeF(pathParts.map(toPart(_))).apply(pathParts)._2.tokens must beEmpty
    }

    def testLeftoverTokensDispPath = forAll(nonEmptyPath,nonEmptyPath) {
      (pathParts: List[String], additional: List[String]) =>
        routeF(pathParts.map(toPart(_))).apply(pathParts ++ additional)._2.dispPath must beEqualTo(additional.mkString("/"))
    }

    def testLeftoverTokensPathTokens = forAll(nonEmptyPath,nonEmptyPath) {
      (pathParts: List[String], additional: List[String]) =>
        routeF(pathParts.map(toPart(_))).apply(pathParts ++ additional)._2.tokens must containAllOf(additional).inOrder
    }
  }

  trait MixedTermsPathDataShared {
    this: MixedRoutesShared =>

    def testDispPathAlwaysEmpty = forAll(pathAndDataPartIdxs) {
      (data: (List[String],Set[Int])) => {
        val (pathParts,dataIdxs) = data
        routeF(buildMixedRouteTerms(pathParts,dataIdxs)).apply(pathParts)._2.dispPath must beEmpty
      }
    }

    def testPathTokensAlwaysEmpty = forAll(pathAndDataPartIdxs) {
      (data: (List[String],Set[Int])) => {
        val (pathParts,dataIdxs) = data
        routeF(buildMixedRouteTerms(pathParts,dataIdxs)).apply(pathParts)._2.tokens must beEmpty
      }
    }

  }
  
  trait AllStringsPathDataShared extends PathDataShared {
    val toPart: String => RoutePart = StringPart(_)
  }
  
  trait AllDataPathDataShared extends PathDataShared {
    def toPart: String => RoutePart = s => DataPart(Symbol(s))
  }
  
  trait MixedRoutesShared  {
    def routeF: List[RouteTerm] => Route

    def testMatchesIIFStringPartsMatch = forAll(pathAndDataPartIdxs) {
      (data: (List[String],Set[Int])) => {
        val (pathParts,dataIdxs) = data
        routeF(buildMixedRouteTerms(pathParts,dataIdxs)).isDefinedAt(pathParts) must beTrue
      }
    }

    def testLessTokensThanParts = forAll(pathDataPartIdxsAndDropCount) {
      (data: (List[String],Set[Int],Int)) => {
        val (pathParts, dataIdxs,dropCount) = data
        routeF(buildMixedRouteTerms(pathParts,dataIdxs))
          .isDefinedAt(pathParts.reverse.drop(dropCount).reverse) must beFalse
      }
    }

    def testStringPartDoesntMatch = forAll(pathDataPartIdxsAndChange) {
      (data: (List[String],Set[Int],Int,String)) => {
        val (pathParts,dataIdxs,changeAt,changeTo) = data
        val changedParts = pathParts.toBuffer
        changedParts.update(changeAt, changeTo)
        routeF(buildMixedRouteTerms(pathParts,dataIdxs)).isDefinedAt(changedParts.toList) must beFalse
      }
    }
       
  }

  object MixedRoutesWithStar extends MixedRoutesShared with MixedTermsPathDataShared {
    val routeF: List[RouteTerm] => Route = routeStartingWith(_, null)

    def testMatchesWithLeftoverTokens = forAll(pathAndDataPartIdxs,nonEmptyPath) {
      (data: (List[String],Set[Int]), additional: List[String]) => {
        val (pathParts,dataIdxs) = data
        routeStartingWith(pathParts.zipWithIndex.map {
          case (s,idx) if dataIdxs.contains(idx) => DataPart(Symbol(s))
          case (s,_) => StringPart(s)
        }, null).isDefinedAt(pathParts ++ additional) must beTrue
      }
    }

    def testLeftoverTokensDispPath = forAll(pathAndDataPartIdxs,nonEmptyPath) {
      (data: (List[String],Set[Int]), additional: List[String]) => {
        val (pathParts,dataIdxs) = data
        routeF(buildMixedRouteTerms(pathParts,dataIdxs)).apply(pathParts ++ additional)._2.dispPath must beEqualTo(additional.mkString("/"))
      }
    }

    def testLeftoverTokensPathTokens = forAll(pathAndDataPartIdxs,nonEmptyPath) {
      (data: (List[String],Set[Int]), additional: List[String]) => {
        val (pathParts,dataIdxs) = data
        routeF(buildMixedRouteTerms(pathParts,dataIdxs)).apply(pathParts ++ additional)._2.tokens must containAllOf(additional).inOrder
      }
    }
  }

  object MixedRoutesNoStar extends MixedRoutesShared with MixedTermsPathDataShared {
    val routeF: List[RouteTerm] => Route = routeMatching(_, null)   

    def testMoreTokensThanParts = forAll(pathAndDataPartIdxs,nonEmptyPath) {
      (data: (List[String],Set[Int]), additional: List[String]) => {
        val (pathParts,dataIdxs) = data
        routeMatching(buildMixedRouteTerms(pathParts,dataIdxs), null).isDefinedAt(pathParts ++ additional) must beFalse
      }
    }

  }
  
  def testMixedRoutesPathInfo = forAll(for { (a,b) <- pathAndDataPartIdxs;c <- Arbitrary.arbitrary[Boolean] } yield (a,b,c)) {
    (data: (List[String],Set[Int],Boolean)) => {
      val (pathParts,dataIdxs,bool) = data
      val terms = buildMixedRouteTerms(pathParts,dataIdxs)
      val route = if (bool) routeMatching(terms,null) else routeStartingWith(terms,null)
      val expected = pathParts.zipWithIndex.filter(t => dataIdxs.contains(t._2)).map(tpl => (Symbol(tpl._1),tpl._1))
      route.apply(pathParts)._2.info must containAllOf(expected).only
    }
  }

  object AllDataRouteWithStar extends AllDataPathDataShared {
    val routeF: List[RouteTerm] => Route = routeStartingWith(_,null)

    def testWithLeftoverTokens = forAll(nonEmptyPath,nonEmptyPath) {
      (pathParts: List[String], additional: List[String]) =>
        routeStartingWith(pathParts.map(s => DataPart(Symbol(s))), null).isDefinedAt(pathParts ++ additional) must beTrue
    }

    def testLessTokensThanParts = lessTokensThanParts(ls => routeStartingWith(ls.map(s => DataPart(Symbol(s))),null))

  }


  object AllDataRouteNoStar extends AllDataPathDataShared {    
    val routeF: List[RouteTerm] => Route = routeMatching(_,null)

    def testEqualLengths = check {
      (pathParts: List[String]) =>
        routeMatching(pathParts.map(s => DataPart(Symbol(s))), null).isDefinedAt(pathParts) must beTrue
    }

    def testUnequalLengths = forAll(differingLists) {
      (lists: (List[String],List[String])) => {
        val (ls1,ls2) = lists
        routeMatching(ls1.map(s => DataPart(Symbol(s))), null).isDefinedAt(ls2) must beFalse
      }
    }
  }

  def testStringRoutesAlwaysHaveEmptyPathInfo = check {
    (pathParts: List[String], bool: Boolean) => {
      val terms = pathParts.map(StringPart(_))
      val route = if (bool) routeMatching(terms,null) else routeStartingWith(terms,null)
      route.apply(pathParts)._2.info must beEmpty
    }      
  }
  
  object AllStringRoutesWithStar extends AllStringsPathDataShared {
    val routeF: List[RouteTerm] => Route = routeStartingWith(_,null)
    
    def testExactStartingWithMatch = check {
      (pathParts: List[String]) => routeStartingWith(pathParts.map(StringPart(_)), null).isDefinedAt(pathParts) must beTrue
    }

    def testLeftoverTokensStillMatch = forAll(nonEmptyPath,nonEmptyPath) {
      (pathParts: List[String], additional: List[String]) =>
        routeStartingWith(pathParts.map(StringPart(_)), null).isDefinedAt(pathParts ++ additional) must beTrue
    }

    def testPartDoesntMatch = atleastOnePartDoesntMatch(ls => routeStartingWith(ls.map(StringPart(_)), null))

    def testLessTokensThanParts = lessTokensThanParts(ls => routeStartingWith(ls.map(StringPart(_)),null))

  }

  object AllStringRoutesNoStar extends AllStringsPathDataShared {
    
    val routeF: List[RouteTerm] => Route = routeMatching(_,null)

    def testExactMatchingPath = check {
      (pathParts: List[String])  => routeMatching(pathParts.map(StringPart(_)), null).isDefinedAt(pathParts) must beTrue
    }

    def testPartDoesntMatch = atleastOnePartDoesntMatch(ls => routeMatching(ls.map(StringPart(_)), null))

    def testLessTokensThanParts = lessTokensThanParts(ls => routeMatching(ls.map(StringPart(_)),null))

    def testMoreTokensThanParts = forAll(nonEmptyPath,nonEmptyPath) {
      (pathParts: List[String], additional: List[String]) =>
        routeMatching(pathParts.map(StringPart(_)), null).isDefinedAt(pathParts ++ additional) must beFalse
    }

  }

  // ScalaCheck Generator for non empty List[String]
  val nonEmptyStr  = Gen.alphaStr.suchThat(_.size > 0)
  val nonEmptyPath = Gen.containerOf[List,String](nonEmptyStr) suchThat { _.size > 0 }

  // generates a nonempty path, an index to change and a value to change that index to
  val pathAndChangeIndexAndValue =
    for {
      ls <- nonEmptyPath
      n <- Gen.choose(0,ls.size - 1)
      s <- nonEmptyStr
    } yield (ls, n, s)

  // generates a non empty path and the number of elements to drop from that path
  val pathAndDropCount =  for { ls <- nonEmptyPath; n <- Gen.choose(1,ls.size - 1) } yield (ls, n)

  // generates lists of differing sizes
  val differingLists = for {
    ls1 <- nonEmptyPath
    ls2 <- nonEmptyPath
    shouldAdd <- Arbitrary.arbitrary[Boolean]
  } yield {
    val (baseList,changeList) = if (ls1.size > ls2.size) (ls1,ls2) else (ls2,ls1)
    if (shouldAdd) (baseList,baseList ++ changeList) else (baseList,baseList drop changeList.size)
  }

  // generates a path and a set of indexes to be intended to be used as data parts
  val pathAndDataPartIdxs = for {
    ls <- nonEmptyPath    
    idxs <- {
      val lsSize = ls.size
      Gen.containerOf1[Set,Int](Gen.choose(0,lsSize - 1)) suchThat { _.size < lsSize }
    }
  } yield (ls, idxs)
  
  // generates a path, a set of indexes inteded to be data parts an index to change that is not one of the data part indexes
  // and a value to change to
  val pathDataPartIdxsAndChange = for {
    (ls,idxs) <- pathAndDataPartIdxs
    n <- Gen.choose(0,ls.size - 1).suchThat(n => !(idxs.contains(n)))
    s <- nonEmptyStr
  } yield (ls,idxs,n,s)

  val pathDataPartIdxsAndDropCount = for {
    (ls,idxs) <- pathAndDataPartIdxs
    n <- Gen.choose(1,ls.size-1)
  } yield (ls,idxs,n)

  val buildMixedRouteTerms: (List[String],Set[Int]) => List[RouteTerm] =
    (pathParts, dataIdxs) => pathParts.zipWithIndex.map {
      case (s,idx) if dataIdxs.contains(idx) => DataPart(Symbol(s))
      case (s,_) => StringPart(s)
    }

  def atleastOnePartDoesntMatch(routeF: List[String] => Route) = forAll(pathAndChangeIndexAndValue) {
    (data: (List[String],Int,String)) => {
      val (pathParts,changeAt,changeTo) = data
      val changedParts = pathParts.toBuffer
      changedParts.update(changeAt, changeTo)
      routeF(pathParts).isDefinedAt(changedParts.toList) must beFalse
    }
  }

  def lessTokensThanParts(routeF: List[String] => Route) = forAll(pathAndDropCount) {
    (data: (List[String],Int)) => {
      val (pathParts, dropCount) = data
      routeF(pathParts).isDefinedAt(pathParts.reverse.drop(dropCount).reverse) must beFalse
    }

  }

}
