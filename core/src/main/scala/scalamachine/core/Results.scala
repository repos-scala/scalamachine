package scalamachine.core

import scalamachine.internal.scalaz.{Functor, Monad}


/**
 * a `Res[A]` is returned by ever [[scalamachine.core.Resource]] function.
 * It is used to return values used by Scalamachine in addition to encoding
 * the ability to continue with or halt the webmachine flow. `Res[A]` is
 * similar to `Option[A]` in that it may or may not contain a value. However,
 * unlike `Option` which can be either `Some` or `None`, a `Res` can be
 * one of `ValueRes`, `HaltRes`, `ErrorRes`, or `EmptyRes`.
 *
 * To create instances of `Res` use [[scalamachine.core.Res.result]], [[scalamachine.core.Res.halt]],
 * [[scalamachine.core.Res.error]], and [[scalamachine.core.Res.empty]]
 *
 * @tparam A - the type of the value that may be contained in this `Res`
 * @see [[scalamachine.core.ResOps]]
 */
sealed trait Res[+A]

/**
 * The existence of a value, similar to `Some`. In addition
 * it signals to the webmachine flow runner to continue to the next
 * decision.
 * @param value - the value contained within this `Res`
 * @tparam A - the type of the value that may be contained in this `Res`
 */
case class ValueRes[+A](value: A) extends Res[A]

/**
 * Signals the webmachine flow the desire to halt with a given
 * response code and possibly a response body. Returning a `HaltRes`
 * will always stop flow runner after the current decision has been completed
 * @param code - response code to be returned
 * @param body - optional response body to be set
 */
case class HaltRes(code: Int, body: Option[HTTPBody] = None) extends Res[Nothing]

/**
 * Signals the webmachine flow the desire to halt with a `500 Internal Server Error`
 * response code with a given response body. Returning an `ErrorRes`
 * will always stop the flow runner after the current decision has been completed
 * @param errorBody - body to be set in response
 */
case class ErrorRes(errorBody: HTTPBody) extends Res[Nothing]

/**
 * Signals the webmachine flow the desire to halt, returning the response
 * without modifying the existing response data in any way. Returning `EmptyRes`
 * will always stop the flow runner after the current decision has been completed
 */
case object EmptyRes extends Res[Nothing]

trait ResOps[A] {
  def res: Res[A]

  /**
   * Returns a new `Res` containing result of apply f to the `Res`'s value, if it exists
   * @param f - function applied if `Res` is nonempty
   * @tparam B - type of value contained by new `Res`
   */
  def map[B](f: A => B): Res[B] = {
    res match {
      case ValueRes(r) => ValueRes(f(r))
      case ErrorRes(e) => ErrorRes(e)
      case HaltRes(c,b)  => HaltRes(c,b)
      case EmptyRes => EmptyRes
    }
  }

  /**
   * Returns a new `Res` containing result of applying `f` to
   * value contained in this `Res`, if the value exists
   * @param f - function applied if `Res` is nonempty
   * @tparam B - type of value contained by new `Res`
   */
  def flatMap[B](f: A => Res[B]): Res[B] = res match {
    case ValueRes(v) => f(v)
    case ErrorRes(e) => ErrorRes(e)
    case HaltRes(c,b) => HaltRes(c,b)
    case EmptyRes => EmptyRes
  }

  /**
   * returns a new `Res` containing the value if the value
   * exists and applying the predicate to the value returns
   * `true`. Otherwise an `EmptyRes` is returned
   */
  def filter(predicate: A => Boolean): Res[A] = res match {
    case ValueRes(a) => if (predicate(a)) ValueRes(a) else EmptyRes
    case ErrorRes(e) => ErrorRes(e)
    case HaltRes(c,b) => HaltRes(c,b)
    case EmptyRes => EmptyRes
  }


  /**
   * Returns new `Res` containing value of this res if it exists,
   * otherwise the other `Res` is returned
   * @param other - the `Res` to return if the other is empty
   */
  def orElse[B >: A](other: Res[B]): Res[B] = res match {
    case ValueRes(r) => ValueRes(r)
    case _ => other
  }

  /**
   * returns the value contained in the `Res` if it is nonempty
   * or the default otherwise
   * @param default - value to return is `Res` is empty
   */
  def getOrElse[B >: A](default: => B) = res match {
    case ValueRes(a) => a
    case _ => default
  }

  /**
   * Turns a `Res[Res[A]]` into a `Res[A]`
   */
  def flatten[B](implicit ev: A <:< Res[B]): Res[B] = flatMap(a => a)

  /**
   * @see [[scalamachine.core.ResOps.getOrElse]]
   */
  def |(default: => A) = getOrElse(default)

  /**
   * Returns `Some` containing the value, if it exists.
   * Otherwise, `None` is returned
   */
  def toOption: Option[A] = res match {
    case ValueRes(a) => Some(a)
    case _ => None
  }
}


object Res extends ResFunctions with ResInternalInstances {

  implicit def resOps[T](r: Res[T]): ResOps[T] = new ResOps[T] {
    def res: Res[T] = r
  }

}

trait ResFunctions {
  def result[A](a: A): Res[A] = ValueRes(a)
  def halt[A](code: Int): Res[A] = HaltRes(code)
  def halt[A](code: Int, body: HTTPBody): Res[A] = HaltRes(code, Option(body))
  def halt[A](code: Int, err: Throwable): Res[A] = HaltRes(code, Option(err.getMessage))
  def error[A](body: HTTPBody): Res[A] = ErrorRes(body)
  def error[A](e: Throwable): Res[A] = error(e.getMessage)
  def empty[A]: Res[A] = EmptyRes
}

trait ResInternalInstances {
  import scalamachine.internal.scalaz.{Monad, Traverse, Applicative}
  implicit val resScalazInternalInstances = new Traverse[Res] with Monad[Res] {
    def point[A](a: => A): Res[A] = ValueRes(a)
    def traverseImpl[G[_],A,B](fa: Res[A])(f: A => G[B])(implicit G: Applicative[G]): G[Res[B]] =
      map(fa)(a => G.map(f(a))(ValueRes(_): Res[B])) match {
        case ValueRes(r) => r
        case HaltRes(c, b) => G.point(HaltRes(c,b))
        case ErrorRes(e) => G.point(ErrorRes(e))
        case _ => G.point(EmptyRes)
      }
    def bind[A, B](fa: Res[A])(f: A => Res[B]): Res[B] = fa flatMap f
  }
}

case class ResTransformer[M[_],A](run: M[Res[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[M]): ResTransformer[M,B] = {
    ResTransformer(F.map(self.run)((_: Res[A]) map f))
  }

  def flatMap[B](f: A => ResTransformer[M,B])(implicit M: Monad[M]) = {
    ResTransformer(M.bind(self.run) {
      case ValueRes(v) => f(v).run
      case r @ HaltRes(_,_) => M.point(r: Res[B])
      case r @ ErrorRes(_) => M.point(r: Res[B])
      case r @ EmptyRes => M.point(r: Res[B])
    })
  }

  def filter(p: A => Boolean)(implicit M: Monad[M]) = {
    ResTransformer(M.bind(self.run) { res => M.point(res filter p) })
  }
}


object ResTransformer extends ResTransformerFunctions with ResTransformerInstances

trait ResTransformerFunctions {
  import scalamachine.internal.scalaz.~>
  def resT[M[_]] = new (({type λ[α] = M[Res[α]]})#λ ~> ({type λ[α] = ResTransformer[M, α]})#λ) {
    def apply[A](a: M[Res[A]]) = ResTransformer[M, A](a)
  }
}

trait ResTransformerInstances {
  import scalamachine.internal.scalaz.{MonadTrans, Pointed}
  import scalamachine.internal.scalaz.syntax.pointed._

  implicit def RTInstances[M[_] : Monad] = new Monad[({type R[X]=ResTransformer[M,X]})#R] {
    def point[A](a: => A): ResTransformer[M,A] = ResTransformer[M,A](Pointed[M].point(Pointed[Res].point(a)))
    def bind[A,B](fa: ResTransformer[M,A])(f: A => ResTransformer[M,B]): ResTransformer[M,B] = fa flatMap f
  }

  implicit val RTMonadTrans = new MonadTrans[ResTransformer] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): ResTransformer[G,A] =
      ResTransformer[G,A](G.map(ga)(_.point[Res]))

    implicit def apply[G[_]: Monad]: Monad[({type R[X]=ResTransformer[G,X]})#R] = RTInstances[G]
  }
}

sealed trait AuthResult {
  def fold[T](success: T, failure: String => T): T = this match {
    case AuthSuccess => success
    case AuthFailure(s) => failure(s)
  }
}
case object AuthSuccess extends AuthResult
case class AuthFailure(headerValue: String) extends AuthResult
