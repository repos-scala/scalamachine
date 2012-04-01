package com.github.jrwest.scalamachine.core



sealed trait Res[+A]
// change to traits with apply, and equal/show instances?
case class ValueRes[+A](value: A) extends Res[A] {
  val isEmpty = false
}
case class ErrorRes(error: Any) extends Res[Nothing] {
  val isEmpty = true
}
case class HaltRes(code: Int) extends Res[Nothing] {
  val isEmpty = true
}
case object EmptyRes extends Res[Nothing] {
  val isEmpty = true
}

trait ResOps[A] {
  def res: Res[A]

  def getOrElse[B >: A](default: => B) = res match {
    case ValueRes(a) => a
    case _ => default
  }

  def |(default: => A) = getOrElse(default)
}


object Res {
  import scalaz.{Monad, Traverse, Applicative}
  implicit val resScalazInstances = new Traverse[Res] with Monad[Res] {
    def point[A](a: => A): Res[A] = ValueRes(a)
    def traverseImpl[G[_],A,B](fa: Res[A])(f: A => G[B])(implicit G: Applicative[G]): G[Res[B]] =
      map(fa)(a => G.map(f(a))(ValueRes(_): Res[B])) match {
        case ValueRes(r) => r
        case HaltRes(c) => G.point(HaltRes(c))
        case ErrorRes(e) => G.point(ErrorRes(e))
        case _ => G.point(EmptyRes)
      }
    def bind[A, B](fa: Res[A])(f: A => Res[B]): Res[B] = fa match {
      case ValueRes(v) => f(v)
      case ErrorRes(e) => ErrorRes(e)
      case HaltRes(c) => HaltRes(c)
      case EmptyRes => EmptyRes
    }
  }

  implicit def resOps[T](r: Res[T]): ResOps[T] = new ResOps[T] {
    def res: Res[T] = r
  }
}

object ValueRes {
  import scalaz.Monad
  // Shouldn't be necessary but it is in order to assuage type inferencer in some cases
  // and sometimes useful when you only want to work withing ValueRes structure
  implicit val valueResScalazInstances = new Monad[ValueRes] {
    def point[A](a: => A): ValueRes[A] = ValueRes(a)
    def bind[A, B](fa: ValueRes[A])(f: A => ValueRes[B]): ValueRes[B] = f(fa.value)
  }
}

sealed trait AuthResult
case object AuthSuccess extends AuthResult
case class AuthFailure(headerValue: String) extends AuthResult

object AuthResult {
  implicit def authResult2Ops(r: AuthResult): AuthResultOps = new AuthResultOps(r)
}

class AuthResultOps(r: AuthResult) {
  def fold[T](success: T, failure: String => T): T = r match {
    case AuthSuccess => success
    case AuthFailure(s) => failure(s)
  }
} 