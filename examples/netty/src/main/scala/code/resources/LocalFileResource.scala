package code.resources

import com.github.jrwest.scalamachine.core._
import java.io.{FileReader, File}

class LocalFileResource extends Resource {
  import Res._

  private val chunkSize = 1024

  override def contentTypesProvided(data: ReqRespData) = {
    val provided = List((ContentType("text/plain"), renderFile(_: ReqRespData)))
    (data, result(provided))
  }

  private def renderFile(data: ReqRespData): (ReqRespData, Res[HTTPBody]) = {
    val file = new File("examples/netty/src/main/scala/code/resources/LocalFileResource.scala")
    val body = LazyStreamBody(
      initialize = new FileReader(file),
      produce = (reader: FileReader) => {
        val characters = new Array[Char](chunkSize)
        try {
          Thread.sleep(25)
          val numRead = reader.read(characters)
          println("READ BYTES: %d" format numRead)
          if (numRead < 0) HTTPBody.EOFChunk
          else HTTPBody.ByteChunk(characters.slice(0, numRead).map(_.toByte))
        } catch {
          case e => HTTPBody.ErrorChunk(e)
        }
      })
    (data, result(body))
  }


}