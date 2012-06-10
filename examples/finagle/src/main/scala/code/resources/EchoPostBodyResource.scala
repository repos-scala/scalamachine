package code.resources

import com.github.jrwest.scalamachine.core._

class EchoPostBodyResource extends Resource {
  import Res._
  import HTTPMethods._

  override def allowedMethods(data: ReqRespData): (ReqRespData, Res[List[HTTPMethod]]) = {
    (data, result(POST :: Nil))
  }

  override def processPost(data: ReqRespData): (ReqRespData, Res[Boolean]) = {
    (data.copy(responseBody = data.requestBody), result(true))
  }

}