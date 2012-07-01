package scalamachine.scalaz.res

import scalaz._
import Scalaz._
import scalamachine.core.{Res, ValueRes, HaltRes, ErrorRes, EmptyRes}
import Res._

case class ResT[M[_], A](run: M[Res[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[M]): ResT[M, B] = {
    ResT(F.fmap(self.run, (_: Res[A]) map f))
  }

  def flatMap[B](f: A => ResT[M, B])(implicit M: Monad[M]) = {
    ResT(M.bind(self.run, (_: Res[A]) match {
      case ValueRes(v) => f(v).run
      case r@HaltRes(_,_) => M.pure(r: Res[B])
      case r@ErrorRes(_) => M.pure(r: Res[B])
      case r@EmptyRes => M.pure(r: Res[B])
    }))
  }

  def filter(p: A => Boolean)(implicit M: Monad[M]) = {
    ResT(M.bind(self.run, (res: Res[A]) => M.pure(res filter p)))
  }

  def orElse[B >: A](other: ResT[M, B])(implicit M: Monad[M]): ResT[M,B] =
    ResT(
      M.bind(self.run, ((_: Res[A])  match {
        case ValueRes(x) => M.pure(ValueRes(x))
        case _ => other.run
      }))
    )
}

object ResT extends ResTFunctions with ResTInstances with ResTSyntax

trait ResTFunctions {
  def resT[M[_]] = new (({type λ[α] = M[Res[α]]})#λ ~> ({type λ[α] = ResT[M, α]})#λ) {
    def apply[A](a: M[Res[A]]) = new ResT[M, A](a)
  }
}

trait ResTInstances {
  implicit def resTPure[M[_]](implicit M: Pure[M]): Pure[({type R[X]=ResT[M,X]})#R] = new Pure[({type R[X]=ResT[M,X]})#R] {
    def pure[A](a: => A): ResT[M,A] = ResT[M,A](M.pure(a.pure[Res]))
  }

  implicit def resTFunctor[M[_]](implicit F: Functor[M]): Functor[({type R[X]=ResT[M,X]})#R] = new Functor[({type R[X]=ResT[M,X]})#R] {
    def fmap[A, B](r: ResT[M,A], f: A => B): ResT[M,B] = r map f
  }

  implicit def resTBind[M[_]](implicit M: Monad[M]): Bind[({type R[X]=ResT[M,X]})#R] = new Bind[({type R[X]=ResT[M,X]})#R] {
    def bind[A,B](ra: ResT[M,A], f: A => ResT[M,B]): ResT[M,B] = ra flatMap f
  }
}

trait ResTSyntax {
  implicit def resToOps[A](ra: Res[A]): ResOps[A] = new ResOps[A] {
    val value = ra
  }

  implicit def maToResTTrans[M[_],A](ma: M[A]): ResTTrans[M,A] = new ResTTrans[M,A] {
    val value = ma
  }
}

sealed trait ResOps[A] extends NewType[Res[A]] {
  import ResT.resT
  def liftT[M[_]](implicit M: Monad[M]): ResT[M, A] = resT[M](value.pure[M])
}

trait ResTTrans[M[_], A] extends NewType[M[A]] {
  import ResT._
  def liftResT(implicit M: Monad[M]): ResT[M,A] = resT[M](M.fmap(value, (_: A).pure[Res]))
}