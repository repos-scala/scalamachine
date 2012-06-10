package com.github.jrwest.scalamachine.netty

import org.jboss.netty.channel.{ChannelFutureListener, MessageEvent, ChannelHandlerContext, SimpleChannelUpstreamHandler}
import org.jboss.netty.handler.codec.http._
import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import scalaz.Id._

class ScalamachineRequestHandler(dispatchTable: DispatchTable[HttpRequest, HttpResponse, Id])
  extends SimpleChannelUpstreamHandler {

  override def messageReceived(ctx: ChannelHandlerContext, evt: MessageEvent) {
    val request = evt.getMessage.asInstanceOf[HttpRequest]

    if (HttpHeaders.is100ContinueExpected(request)) send100Continue(evt)
    else {
      val response: HttpResponse = dispatchTable.lift(request) getOrElse {
        new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
      }

      writeResponse(evt, request, response)
    }
  }

  private def writeResponse(evt: MessageEvent, request: HttpRequest, response: HttpResponse) {
    val keepAlive = HttpHeaders.isKeepAlive(request)

    if (keepAlive) {
      response.setHeader(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.KEEP_ALIVE)
    }

    response.setHeader(HttpHeaders.Names.CONTENT_LENGTH, response.getContent.readableBytes())
    val writeFuture = evt.getChannel.write(response)

    if (!keepAlive) writeFuture.addListener(ChannelFutureListener.CLOSE)
  }

  private def send100Continue(e: MessageEvent) {
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE)
    e.getChannel.write(response)
  }

}