package com.github.jrwest.scalamachine.scalaz.res

import scalaz.{Monad, Functor}
import com.github.jrwest.scalamachine.core.{Res, ValueRes, HaltRes, ErrorRes, EmptyRes}
import Res._

case class ResT[M[_],A](run: M[Res[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[M]): ResT[M,B] = {
    ResT(F.map(self.run)((_: Res[A]) map f))
  }

  def flatMap[B](f: A => ResT[M,B])(implicit M: Monad[M]) = {
    ResT(M.bind(self.run) {
      case ValueRes(v) => f(v).run
      case r @ HaltRes(_) => M.point(r: Res[B])
      case r @ ErrorRes(_) => M.point(r: Res[B])
      case r @ EmptyRes => M.point(r: Res[B])
    })
  }

  def filter(p: A => Boolean)(implicit M: Monad[M]) = {
    ResT(M.bind(self.run) { res => M.point(res filter p) })
  }
}

object ResT extends ResTFunctions

trait ResTFunctions {
  import scalaz.~>
  def resT[M[_]] = new (({type λ[α] = M[Res[α]]})#λ ~> ({type λ[α] = ResT[M, α]})#λ) {
    def apply[A](a: M[Res[A]]) = new ResT[M, A](a)
  }
}