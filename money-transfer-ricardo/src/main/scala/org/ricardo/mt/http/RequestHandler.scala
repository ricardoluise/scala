package org.ricardo.mt.http

import io.netty.handler.codec.http.HttpResponseStatus

import scala.concurrent.Future


trait RequestHandler {
  def handle(req: HttpReq): Future[HttpResp]
}

object RequestHandler {
  def apply(handler: (HttpReq) => Future[HttpResp]) = new RequestHandler {
    def handle(req: HttpReq): Future[HttpResp] = handler(req)
  }
}

object DefaultNotFoundHandler extends RequestHandler {
  def handle(req: HttpReq) = {
    Future.successful(HttpResp(
      status = HttpResponseStatus.NOT_FOUND,
      body = Some(Content(req.uri + " is not found"))
    ))
  }
}

trait PartialRequestHandler {
  /**
   * Partial function should have an efficient applyOrElse method.
   * All internal scala partials already have overridden applyOrElse methods.
   *
   * @return Partial function for request handling
   * @see PartialFunction.applyOrElse
   */
  def tryHandle: PartialFunction[HttpReq, Future[HttpResp]]
}

object PartialRequestHandler {
  def apply(handler: PartialFunction[HttpReq, Future[HttpResp]]) = new PartialRequestHandler {
    def tryHandle = handler
  }
}

trait NestedPartialRequestHandler extends RequestHandler {
  def handlers: Seq[PartialRequestHandler]
  def fallbackHandler: RequestHandler = DefaultNotFoundHandler

  def handle(req: HttpReq) = {
    // We use view to have a lazy collection and do not call extra handlers if match was found
    // We use lift because Lifted uses applyOrElse which is efficient for partial functions
    handlers.view.flatMap(_.tryHandle.lift(req)).headOption.getOrElse {
      fallbackHandler.handle(req)
    }
  }
}

object NestedPartialRequestHandler {

  def apply(partialHandlers: Seq[PartialRequestHandler]): NestedPartialRequestHandler = {
    new NestedPartialRequestHandler {
      def handlers = partialHandlers
    }
  }

  def apply(partialHandlers: Seq[PartialRequestHandler],
            fallback: RequestHandler): NestedPartialRequestHandler = {
    new NestedPartialRequestHandler {
      def handlers = partialHandlers

      override def fallbackHandler = fallback
    }
  }
}
