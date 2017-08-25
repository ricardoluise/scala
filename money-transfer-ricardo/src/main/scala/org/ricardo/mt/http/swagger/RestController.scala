package org.ricardo.mt.http.swagger

import io.netty.handler.codec.http.HttpResponseStatus._
import io.netty.handler.codec.http.{HttpMethod, HttpResponseStatus}
import org.ricardo.mt.http._
import org.ricardo.mt.http.path._
import org.ricardo.mt.util.Loggable
import play.api.libs.json._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._

trait RestController extends Loggable {
  type CallbackHandler[I, O] = ApiRequest[I] => Future[O]
  type Handler = (MethodRequest) => Future[HttpResp]

  implicit protected def execctx: ExecutionContext

  private var methodBuffer = ArrayBuffer[(OperationDefinition, Handler)]()
  lazy val operations: Seq[(OperationDefinition, Handler)] = methodBuffer.toIndexedSeq

  protected val POST    = MethodBuilder(HttpMethod.POST, Path())
  protected val PUT     = MethodBuilder(HttpMethod.PUT, Path())
  protected val GET     = MethodBuilder(HttpMethod.GET, Path())
  protected val DELETE  = MethodBuilder(HttpMethod.DELETE, Path())

  protected def intParam(name: String) = QueryParameter(name, Primitive.INT)
  protected def strParam(name: String) = QueryParameter(name, Primitive.STRING)

  implicit protected def dataCallback[I,O](pureHandler: CallbackHandler[I, O])
                                          (implicit out: TypeTag[O], writes: Writes[O], in: TypeTag[I], reads: Reads[I]): Callback = {
    val handler = {req: MethodRequest => {
      pureHandler(bindRequest[I](req)).map { data =>
        val string1 = Json.toJson(data).toString()
        HttpResp(OK,
          body = Some(Content(string1)),
          headers = Map(ContentType(_.JSON))
        )
      }.recover {
        case e: ApiError =>
          HttpResp(e.code,
            body = Some(Content(Json.toJson(e.getMessage).toString())),
            headers = Map(ContentType(_.JSON))
          )
        case e =>
          log.warn(s"Error on $req", e)
          HttpResp(INTERNAL_SERVER_ERROR)
      }
    }}

    Callback(handler, in.tpe, out.tpe)
  }

  private def bindRequest[I](req: MethodRequest)(implicit in: TypeTag[I], reads: Reads[I]): ApiRequest[I] = in.tpe match {
    case u if u =:= typeOf[Unit] => ApiRequest(Unit.asInstanceOf[I], req.params)
    case o if o <:< typeOf[Option[Any]] => ApiRequest(
      if (req.http.content.hasRemaining) parseRequest[I](req) else None.asInstanceOf[I],
      req.params
    )
    case _ => ApiRequest(parseRequest[I](req), req.params)
  }

  private def parseRequest[I](req: MethodRequest)(implicit in: TypeTag[I], reads: Reads[I]) = {
    val json = req.http.method match {
      case HttpMethod.GET | HttpMethod.DELETE =>
        val map = req.http.query.map { case (name, values) =>
          name -> JsString(values.lastOption.getOrElse(""))
        }
        Json.toJson(JsObject(map.toSeq))
      case HttpMethod.POST | HttpMethod.PUT | HttpMethod.PATCH => Json.parse(req.http.contentString)
      case m => throw new IllegalArgumentException(s"Unsupported method $m")
    }

    json.as[I](reads)
  }

  case class QueryParameter(name: String, dataType: FieldType)

  case class MethodBuilder(httpMethod: HttpMethod, path: Path) {
    def /(s: Segment) = MethodBuilder(httpMethod, path / s)
    def :?(q: QueryParameter) = MethodParamBuilder(this, Seq(q))
    def &(q: QueryParameter) = MethodParamBuilder(this, Seq(q))
    def will(desc: String)  = MethodHandlerBuilder(MethodParamBuilder(this), desc)
  }
  case class MethodParamBuilder(mb: MethodBuilder, params: Seq[QueryParameter] = Seq.empty) {
    def will(desc: String)  = MethodHandlerBuilder(this, desc)
    def &(q: QueryParameter) = MethodParamBuilder(mb, params :+ q)
  }

  case class MethodHandlerBuilder(dmb: MethodParamBuilder, desc: String) {

    def :=(callback: Callback) = {
      methodBuffer :+= (OperationDefinition(Method(dmb.mb.httpMethod, dmb.mb.path), callback.in, callback.out, callback.errors, desc) -> callback.handler)
    }

  }

  case class Callback(handler: Handler, in: Type, out: Type, errors: Map[HttpResponseStatus, String] = Map.empty) {
    def errors(errors: (HttpResponseStatus, String)*) = copy(errors = errors.toMap)
  }

}

case class ApiRequest[I](data: I, params: Map[String, Any]) {
  def param[A](name: String) = params(name).asInstanceOf[A]
  def paramOpt[A](name: String) = params.get(name).map(v => v.asInstanceOf[A])
}

case class Method(httpMethod: HttpMethod, path: Path) extends Ordered[Method] {
  def name = s"${httpMethod.toString.toLowerCase()}.${path.toString.replace('/', '.')}"

  override def compare(that: Method) = {
    httpMethod.compareTo(that.httpMethod) match {
      case 0 => path.toString.compareTo(that.path.toString)
      case i => i
    }
  }
}
case class MethodRequest(method: Method, http: HttpReq, params: Parameters)

class ApiError(val code: HttpResponseStatus, message: String) extends RuntimeException(message)
