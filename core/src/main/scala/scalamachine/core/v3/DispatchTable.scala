package scalamachine.core
package v3

import dispatch.DispatchTable


trait V3DispatchTable[-A, B, +W[_]] extends DispatchTable[A, B, W] with WebmachineDecisions {
  val firstDecision = b13
}