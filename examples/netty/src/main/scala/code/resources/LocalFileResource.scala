package code.resources

import scalamachine.core._
import java.io.{FileReader, File}

class LocalFileResource extends Resource {
  import Res._

  private val chunkSize = 1024

  override def contentTypesProvided(data: ReqRespData) = {
    val provided = List((ContentType("text/plain"), renderFile(_: ReqRespData)))
    (data, result(provided))
  }

  private def renderFile(data: ReqRespData): (ReqRespData, Res[HTTPBody]) = {
    val path = data.query.get("path").flatMap(_.headOption).getOrElse("examples/netty/src/main/scala/code/resources/LocalFileResource.scala")
    val file = new File(path)
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
      },
      ensuring = (r: FileReader) => { println("CLOSING FILE"); r.close() }
    )
    (data, result(body))
  }


}