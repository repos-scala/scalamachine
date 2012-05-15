package com.github.jrwest.scalamachine.finagle

import com.twitter.finagle.Service
import org.jboss.netty.handler.codec.http.{HttpResponse, HttpRequest}
import com.twitter.util.Future
import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import org.slf4j.LoggerFactory

class FinagleWebmachineService(dispatchTable: DispatchTable[HttpRequest,HttpResponse,Future])
  extends Service[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest): Future[HttpResponse] = {
    if (dispatchTable.isDefinedAt(request)) dispatchTable(request)
    else Future.exception(new Throwable("four-oh-four"))
  }
}