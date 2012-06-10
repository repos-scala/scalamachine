package com.github.jrwest.scalamachine.internal

package object ext {
  implicit def F1ToSt[A,B](func: A => (A,B)): Function1ToState[A,B] = new Function1ToState[A,B] {
    val f = func
  }
}