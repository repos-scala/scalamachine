package com.github.jrwest.scalamachine.finagle

import org.jboss.netty.handler.codec.http.{HttpResponse, HttpRequest}
import com.twitter.finagle.{Service, SimpleFilter}
import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import com.twitter.util.Future

class FinagleWebmachineFilter(dispatchTable: DispatchTable[HttpRequest,HttpResponse,Future])
  extends SimpleFilter[HttpRequest, HttpResponse] {
  def apply(request: HttpRequest, continue: Service[HttpRequest,HttpResponse]) = {
    if (dispatchTable.isDefinedAt(request)) dispatchTable(request)
    else continue(request)
  }
}