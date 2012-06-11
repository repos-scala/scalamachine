package com.github.jrwest.scalamachine.finagle

import com.twitter.finagle.Service
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpRequest}
import com.twitter.util.Future
import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import com.github.jrwest.scalamachine.netty.{FixedLengthResponse, NettyHttpResponse}

class FinagleWebmachineService(dispatchTable: DispatchTable[HttpRequest,NettyHttpResponse,Future])
  extends Service[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest): Future[HttpResponse] = {
    if (dispatchTable.isDefinedAt(request)) dispatchTable(request) flatMap {
      case FixedLengthResponse(r) => Future(r)
      case _ => Future.exception(new Throwable("scalamachine-finagle does not support streaming responses"))
    }
    else Future.exception(new Throwable("four-oh-four"))
  }
}