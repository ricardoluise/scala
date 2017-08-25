package org.ricardo.mt.http.swagger

import org.apache.commons.io.IOUtils
import org.ricardo.mt.http._

import scala.concurrent.Future


class SwaggerUiHandler(basePath: String) extends PartialRequestHandler {
  val RESOURCE_PATH = "io/swagger/ui"
  val INDEX = "index.html"

  val extensions = Map(
    "js" -> ContentType.JS,
    "html" -> ContentType.HTML,
    "css" -> ContentType.CSS,
    "gif" -> ContentType.GIF,
    "png" -> ContentType.PNG,
    "ico" -> ContentType.ICO,
    "ttf" -> ContentType.TTF
  )

  def tryHandle: PartialFunction[HttpReq, Future[HttpResp]] = Function.unlift { req =>
    val base = if (basePath.isEmpty) basePath else s"$basePath/"
    if (req.path.startsWith(base)) {
      val file = req.path.substring(base.length)
      val name = if (file.isEmpty) INDEX else file
      extensions.find { case (e, t) =>
        name.endsWith(s".$e")
      }.flatMap { case (e, t) =>
        resource(name).map { bytes =>
          Future.successful(HttpResp(
            body = Some(Content(bytes)),
            headers = Map(ContentType(t))
          ))
        }
      }
    } else {
      None
    }
  }

  def resource(name: String): Option[Array[Byte]] = {
    Option(getClass.getClassLoader.getResourceAsStream(s"$RESOURCE_PATH/$name")).map(IOUtils.toByteArray)
  }
}
