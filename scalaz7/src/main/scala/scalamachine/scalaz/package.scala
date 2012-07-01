package scalamachine

import _root_.scalaz.State
import core.{Res, ReqRespData}
import scalaz.res.ResT

package object scalaz {

  type ReqRespState[A] = State[ReqRespData,A]
  type ReqRespStateRes[A] = ReqRespState[Res[A]]
  type ReqRespStateResT[A] = ResT[ReqRespState,A]

}