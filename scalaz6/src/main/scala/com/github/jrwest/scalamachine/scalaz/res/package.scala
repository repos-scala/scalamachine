package com.github.jrwest.scalamachine.scalaz

import scalaz._
import Scalaz._
import com.github.jrwest.scalamachine.core._

package object res {
  implicit def resPure: Pure[Res] = new Pure[Res] {
    def pure[A](a: => A): Res[A] = ValueRes(a)
  }

  implicit def resBind: Bind[Res] = new Bind[Res] {
    def bind[A,B](ra: Res[A], f: A => Res[B]): Res[B] = ra flatMap f
  }

  implicit def resTraverse: Traverse[Res] = new Traverse[Res] {
    def traverse[G[_],A,B](f: A => G[B], fa: Res[A])(implicit G: Applicative[G]): G[Res[B]] =
      fmap(fa, (a: A) => G.fmap(f(a), (b: B) => (ValueRes(b): Res[B]))) match {
        case ValueRes(r) => r
        case HaltRes(c) => G.pure(HaltRes(c))
        case ErrorRes(e) => G.pure(ErrorRes(e))
        case _ => G.pure(EmptyRes)
      }
  }
}