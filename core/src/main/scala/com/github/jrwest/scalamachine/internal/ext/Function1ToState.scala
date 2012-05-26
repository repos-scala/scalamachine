package com.github.jrwest.scalamachine.internal
package ext

import scalaz.State

trait Function1ToState[A,B] {
  def f: A => (B,A)

  def st: State[A,B] = State(f)
}