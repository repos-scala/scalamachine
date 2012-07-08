package scalamachine.finagle

import org.jboss.netty.handler.codec.http.{HttpResponse, HttpRequest}
import com.twitter.finagle.{Service, SimpleFilter}
import scalamachine.core.dispatch.DispatchTable
import com.twitter.util.Future
import scalamachine.netty.{FixedLengthResponse, NettyHttpResponse}

class FinagleWebmachineFilter(dispatchTable: DispatchTable[HttpRequest,NettyHttpResponse,Future])
  extends SimpleFilter[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest, continue: Service[HttpRequest,HttpResponse]) = {
    dispatchTable(request).map(_.flatMap {
      case FixedLengthResponse(r) => Future(r)
      case _ => Future.exception(new Exception("scalamachine-finagle does not support streaming responses"))
    }).getOrElse(continue(request))
  }
}