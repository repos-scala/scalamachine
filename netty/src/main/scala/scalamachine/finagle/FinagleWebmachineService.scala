package scalamachine.finagle

import com.twitter.finagle.Service
import org.jboss.netty.handler.codec.http._
import com.twitter.util.Future
import scalamachine.core.dispatch.DispatchTable
import scalamachine.netty.{FixedLengthResponse, NettyHttpResponse}
import scalamachine.netty.FixedLengthResponse

class FinagleWebmachineService(dispatchTable: DispatchTable[HttpRequest,NettyHttpResponse,Future])
  extends Service[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest): Future[HttpResponse] = {
    dispatchTable(request).map(_.flatMap {
      case FixedLengthResponse(r) => Future(r)
      case _ => Future.exception(new Exception("scalamachine-finagle does not support streaming responses"))
    }).getOrElse(Future.value(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)))

  }
}