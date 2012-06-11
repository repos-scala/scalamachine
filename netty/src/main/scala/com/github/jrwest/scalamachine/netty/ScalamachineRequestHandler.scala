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

    lazy val writeChunks = LazyStreamBody.forEachChunkUntilFalse {
      case HTTPBody.ByteChunk(bytes) => {
        val chunk = new DefaultHttpChunk(ChannelBuffers.wrappedBuffer(bytes))
        writeToChannel(evt, chunk).isDefined
      }
      case HTTPBody.ErrorChunk(e) => {
        logger.error("Error Producing Chunk", e)
        writeFinalChunk(evt, keepAlive)
      }
      case HTTPBody.EOFChunk => writeFinalChunk(evt, keepAlive)
    }

    mbChunks map {
      chunks => {
        val doChunkWriting: IO[Unit] = for {
          chunkEnumerator <- chunks
          _ <- (writeChunks &= chunkEnumerator).run
        } yield ()
        doChunkWriting.unsafePerformIO()
      }
    } getOrElse {
      if (!keepAlive) responseWriteFuture.addListener(ChannelFutureListener.CLOSE)
    }
  }

  private def writeToChannel(e: MessageEvent, chunk: HttpChunk): Option[ChannelFuture] = {
    if (e.getChannel.isConnected) {
      Some(e.getChannel.write(chunk))
    } else None
  }

  private def writeFinalChunk(e: MessageEvent, keepAlive: Boolean): Boolean = {
    writeToChannel(e, HttpChunk.LAST_CHUNK) map { future =>
      if (!keepAlive) {
        future.addListener(ChannelFutureListener.CLOSE)
        true
      }
      else true
    } getOrElse false
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