package scalamachine.scalaz

import scalaz._
import Scalaz._
import scalamachine.core._

package object res {
  implicit def resPure: Pure[Res] = new Pure[Res] {
    def pure[A](a: => A): Res[A] = ValueRes(a)
  }

  implicit def resBind: Bind[Res] = new Bind[Res] {
    def bind[A,B](ra: Res[A], f: A => Res[B]): Res[B] = Res.resOps(ra).flatMap(f) // explicitly convert to ops here to disambiguate implicit priority
  }

  implicit def resTraverse: Traverse[Res] = new Traverse[Res] {
    def traverse[G[_],A,B](f: A => G[B], fa: Res[A])(implicit G: Applicative[G]): G[Res[B]] =
      fmap(fa, (a: A) => G.fmap(f(a), (b: B) => (ValueRes(b): Res[B]))) match {
        case ValueRes(r) => r
        case HaltRes(c,b) => G.pure(HaltRes(c,b))
        case ErrorRes(e) => G.pure(ErrorRes(e))
        case _ => G.pure(EmptyRes)
      }

    override def fmap[A, B](ra: Res[A], f: A => B) = Res.resOps(ra).map(f)
  }
}