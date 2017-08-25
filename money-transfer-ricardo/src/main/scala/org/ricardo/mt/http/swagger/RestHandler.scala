package org.ricardo.mt.http.swagger

import org.ricardo.mt.http._
import org.ricardo.mt.http.matching._
import org.ricardo.mt.http.path.Path
import play.api.libs.json.Json

import scala.concurrent.Future


object RestHandler {
  def apply(controllers: Seq[RestController], endpoint: String = "") = new NestedPartialRequestHandler {
    private val controllerHandlers = controllers.map(new ResourceHandler(_))

    private val ui = new SwaggerUiHandler(endpoint)

    private val docs = new PartialRequestHandler {
      private def listingResponse = Json.toJson(
        ResourceListing(controllerHandlers.map(r => Resource(r.basePath.toString, None)), apiVersion = Some("1.0")
      )).toString

      def tryHandle: PartialFunction[HttpReq, Future[HttpResp]] = {
        case GET -> "api-docs" => Future.successful(HttpResp(
          body = Some(Content(listingResponse)),
          headers = Map(ContentType(_.JSON))
        ))
      }
    }

    def handlers: Seq[PartialRequestHandler] = ui +: docs +: controllerHandlers
  }

  class ResourceHandler(controller: RestController, endpoint: String = "/") extends PartialRequestHandler {
    val (basePath, declaration) = {
      val operations = controller.operations.map(_._1)
      val apis = operations.groupBy(_.method.path).toSeq.map { case (path, defs) =>
        Api(path.toString, defs.map(_.operation).sortBy(_.method), None)
      }.sortBy(_.path)

      val models = operations.flatMap(_.models).distinct.sortBy(_.id)

      val basePath = operations.map(_.method.path).foldLeft[Option[Path]](None)((base, path) => base match {
        case Some(x) => Some(x.base(path))
        case None => Some(path)
      }).map(_.staticPrefix).getOrElse(Path())

      basePath -> ApiDeclaration(
        endpoint,
        basePath.toString,
        apis,
        models.toList.map(m => (m.id, m)).toMap
      )
    }

    private val DOC_PATH = "api-docs" + basePath.toString
    private val declarationResponse = Json.toJson(declaration).toString

    def tryHandle: PartialFunction[HttpReq, Future[HttpResp]] = {
      val docs:PartialFunction[HttpReq, Future[HttpResp]] = {
        case GET -> DOC_PATH => Future.successful(HttpResp(
          body = Some(Content(declarationResponse)),
          headers = Map(ContentType(_.JSON))
        ))
      }
      val methods = Function.unlift { req: HttpReq =>
        controller.operations.flatMap { case (op, handler) =>
          if (op.method.httpMethod == req.method) {
            op.method.path.parse(req.path).map(MethodRequest(op.method, req, _) -> handler)
          } else {
            None
          }
        }.headOption.map { case (m, handler) =>
          handler(m)
        }
      }

      docs orElse methods
    }
  }
}
