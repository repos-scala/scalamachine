package code.resources

import com.github.jrwest.scalamachine.core._

class EchoPostBodyResource extends Resource {
  import Res._
  import HTTPMethods._

  override def allowedMethods(data: ReqRespData): (Res[List[HTTPMethod]], ReqRespData) = {
    (result(POST :: Nil), data)
  }

  override def processPost(data: ReqRespData): (Res[Boolean], ReqRespData) = {
    (result(true), data.copy(responseBody = data.requestBody))
  }

}