package com.github.jrwest.scalamachine.finagle

import com.twitter.util.Future
import com.github.jrwest.scalamachine.core.v3.V3DispatchTable
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}
import com.github.jrwest.scalamachine.netty.{NettyHttpResponse, NettyWebmachine}

trait FinagleWebmachine {
  this: NettyWebmachine[Future] =>

  def wrap(res: => NettyHttpResponse) = Future(res)

}

trait FinagleWebmachineV3 extends V3DispatchTable[HttpRequest,NettyHttpResponse,Future] with FinagleWebmachine with NettyWebmachine[Future]