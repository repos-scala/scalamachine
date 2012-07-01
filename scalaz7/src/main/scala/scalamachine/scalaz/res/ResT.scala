package scalamachine.scalaz
package res

import scalamachine.core._
import Res._
import scalaz.{MonadTrans, Pointed, Monad, Functor}
import scalaz.syntax.Ops
import scalamachine.core.ErrorRes
import scalamachine.core.ValueRes
import scalamachine.core.HaltRes

case class ResT[M[_],A](run: M[Res[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[M]): ResT[M,B] = {
    ResT(F.map(self.run)((_: Res[A]) map f))
  }

  def flatMap[B](f: A => ResT[M,B])(implicit M: Monad[M]) = {
    ResT(M.bind(self.run) {
      case ValueRes(v) => f(v).run
      case r @ HaltRes(_,_) => M.point(r: Res[B])
      case r @ ErrorRes(_) => M.point(r: Res[B])
      case r @ EmptyRes => M.point(r: Res[B])
    })
  }

  def filter(p: A => Boolean)(implicit M: Monad[M]) = {
    ResT(M.bind(self.run) { res => M.point(res filter p) })
  }

  def orElse[B >: A](other: ResT[M, B])(implicit M: Monad[M]): ResT[M,B] =
    ResT(
      M.bind(self.run)(_ match {
        case ValueRes(x) => M.point(ValueRes(x))
        case _ => other.run
      })
    )

}

object ResT extends ResTFunctions with ResTInstances with ResTSyntax

trait ResTFunctions {
  import scalaz.~>
  import ResT._
  def resT[M[_]] = new (({type λ[α] = M[Res[α]]})#λ ~> ({type λ[α] = ResT[M, α]})#λ) {
    def apply[A](a: M[Res[A]]) = new ResT[M, A](a)
  }

  def resultT[M[_] : Monad, A](value: A): ResT[M,A] = result(value).liftT[M]
  def haltT[M[_] : Monad, A](code: Int): ResT[M,A] = halt[A](code).liftT[M]
  def haltT[M[_] : Monad, A](code: Int, body: HTTPBody): ResT[M,A] = halt[A](code, body).liftT[M]
  def errorT[M[_] : Monad, A](body: HTTPBody): ResT[M,A] = error[A](body).liftT[M]
  def errorT[M[_] : Monad, A](err: Throwable): ResT[M,A] = error[A](err).liftT[M]
  def emptyT[M[_] : Monad, A]: ResT[M,A] = empty[A].liftT[M]

  def resTRRS[A](v: ReqRespStateRes[A]): ResT[ReqRespState,A] = resT[ReqRespState](v)
}


trait ResTInstances {
  import scalaz.syntax.pointed._

  implicit def resTInstances[M[_] : Monad] = new Monad[({type R[X]=ResT[M,X]})#R] {
    def point[A](a: => A): ResT[M,A] = ResT[M,A](Pointed[M].point(Pointed[Res].point(a)))
    def bind[A,B](fa: ResT[M,A])(f: A => ResT[M,B]): ResT[M,B] = fa flatMap f
  }

  implicit val ResTMonadTrans = new MonadTrans[ResT] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): ResT[G,A] =
      ResT[G,A](G.map(ga)(_.point[Res]))

    implicit def apply[G[_]: Monad]: Monad[({type R[X]=ResT[G,X]})#R] =
      resTInstances[G]
  }
}

trait ResTSyntax {
  implicit def resToOps[A](ra: Res[A]): ResOps[A] = new ResOps[A] {
    val self = ra
  }
}

sealed trait ResOps[A] extends Ops[Res[A]] {
  import ResT.resT
  import scalaz.syntax.pointed._
  def liftT[M[_]](implicit M: Monad[M]): ResT[M, A] = resT[M](self.point[M])
}