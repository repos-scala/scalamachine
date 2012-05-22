package com.github.jrwest.scalamachine.scalaz.res

import scalaz._
import Scalaz._
import com.github.jrwest.scalamachine.core.{Res, ValueRes, HaltRes, ErrorRes, EmptyRes}
import Res._

case class ResT[M[_], A](run: M[Res[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[M]): ResT[M, B] = {
    ResT(F.fmap(self.run, (_: Res[A]) map f))
  }

  def flatMap[B](f: A => ResT[M, B])(implicit M: Monad[M]) = {
    ResT(M.bind(self.run, (_: Res[A]) match {
      case ValueRes(v) => f(v).run
      case r@HaltRes(_) => M.pure(r: Res[B])
      case r@ErrorRes(_) => M.pure(r: Res[B])
      case r@EmptyRes => M.pure(r: Res[B])
    }))
  }

  def filter(p: A => Boolean)(implicit M: Monad[M]) = {
    ResT(M.bind(self.run, (res: Res[A]) => M.pure(res filter p)))
  }
}

object ResT extends ResTFunctions

trait ResTFunctions {
  def resT[M[_]] = new (({type λ[α] = M[Res[α]]})#λ ~> ({type λ[α] = ResT[M, α]})#λ) {
    def apply[A](a: M[Res[A]]) = new ResT[M, A](a)
  }
}
