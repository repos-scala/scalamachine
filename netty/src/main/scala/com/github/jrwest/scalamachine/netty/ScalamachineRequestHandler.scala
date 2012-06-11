package com.github.jrwest.scalamachine.netty

import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http._
import com.github.jrwest.scalamachine.core.dispatch.DispatchTable
import scalaz.Id._
import com.github.jrwest.scalamachine.core.{HTTPBody, LazyStreamBody}
import org.jboss.netty.buffer.ChannelBuffers
import org.slf4j.LoggerFactory
import scalaz.effect.IO

class ScalamachineRequestHandler(dispatchTable: DispatchTable[HttpRequest, NettyHttpResponse, Id])
  extends SimpleChannelUpstreamHandler {

  private val logger = LoggerFactory.getLogger(classOf[ScalamachineRequestHandler])

  override def messageReceived(ctx: ChannelHandlerContext, evt: MessageEvent) {
    val request = evt.getMessage.asInstanceOf[HttpRequest]

    if (HttpHeaders.is100ContinueExpected(request)) send100Continue(evt)
    else {
      val response: NettyHttpResponse = dispatchTable.lift(request) getOrElse {
        FixedLengthResponse(new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND))
      }

      writeResponse(evt, request, response)
    }
  }

  private def writeResponse(evt: MessageEvent, request: HttpRequest, response: NettyHttpResponse) {
    val keepAlive = HttpHeaders.isKeepAlive(request)

    val (finalResponse, mbChunks) = response match {
      case FixedLengthResponse(r) => (prepareResponse(r, keepAlive, false), None)
      case ChunkedResponse(r, chunks) => (prepareResponse(r, keepAlive, true), Some(chunks))
    }

    val responseWriteFuture = evt.getChannel.write(finalResponse)

    // TODO: deal with closed channel
    // TODO: do we need to call write only after the previous one completed?
    lazy val writeChunks = LazyStreamBody.forEachChunk {
      case HTTPBody.ByteChunk(bytes) => {
        logger.debug("writing chunk: %d" format bytes.size)
        val chunk = new DefaultHttpChunk(ChannelBuffers.wrappedBuffer(bytes))
        evt.getChannel.write(chunk)
      }
      case HTTPBody.ErrorChunk(e) => {
        logger.error("Error Streaming Chunks", e)
        val writeTrailerFuture = evt.getChannel.write(HttpChunk.LAST_CHUNK)
        if (!keepAlive) writeTrailerFuture.addListener(ChannelFutureListener.CLOSE)
      }
      case HTTPBody.EOFChunk => {
        logger.debug("writing trailing chunk")
        val writeTrailerFuture = evt.getChannel.write(HttpChunk.LAST_CHUNK)
        if (!keepAlive) writeTrailerFuture.addListener(ChannelFutureListener.CLOSE)
      }
    }

    mbChunks map {
      chunks => {
        val doChunkWriting: IO[Unit] = for {
          chunkEnumerator <- chunks
          _ <- (writeChunks &= chunkEnumerator).run
        } yield ()
        responseWriteFuture.addListener(new ChannelFutureListener {
          def operationComplete(future: ChannelFuture) {
            doChunkWriting.unsafePerformIO()
          }
        })
      }
    } getOrElse {
      responseWriteFuture.addListener(ChannelFutureListener.CLOSE)
    }
  }

  private def prepareResponse(response: HttpResponse, isKeepAlive: Boolean, isChunked: Boolean): HttpResponse = {
    if (isKeepAlive) {
      response.setHeader(HttpHeaders.Names.CONNECTION, HttpHeaders.Values.KEEP_ALIVE)
    }

    if (!isChunked) {
      response.setChunked(false)
      response.setHeader(HttpHeaders.Names.CONTENT_LENGTH, response.getContent.readableBytes())
    } else {
      response.setChunked(true)
      response.setHeader(HttpHeaders.Names.TRANSFER_ENCODING, HttpHeaders.Values.CHUNKED)
    }

    response
  }

  private def send100Continue(e: MessageEvent) {
    val response = new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE)
    e.getChannel.write(response)
  }

}