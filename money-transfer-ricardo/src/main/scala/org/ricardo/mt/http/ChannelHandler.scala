package org.ricardo.mt.http

import io.netty.channel.{ChannelHandlerContext, SimpleChannelInboundHandler}
import io.netty.handler.codec.http.HttpResponseStatus
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

trait ChannelHandler extends SimpleChannelInboundHandler[HttpReq] {
  lazy val log = LoggerFactory.getLogger(getClass)

  def handle(ctx: ChannelHandlerContext, req: HttpReq)

  def channelRead0(ctx: ChannelHandlerContext, req: HttpReq) {
    try {
      handle(ctx, req)
    } catch {
      case NonFatal(e) =>
        this.exceptionCaught(ctx, e)
    }
  }

  override def channelUnregistered(ctx: ChannelHandlerContext) = {
    super.channelUnregistered(ctx)
  }

  override def channelReadComplete(ctx: ChannelHandlerContext) = {
    super.channelReadComplete(ctx)
    ctx.flush()
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = {
    super.channelReadComplete(ctx)
    cause match {
      case NonFatal(e) => log.warn("Http Channel error", e)
    }
    ctx.close()
  }
}


class DefaultChannelHandler(handler: RequestHandler, queryFactory: (ChannelHandlerContext, HttpReq) => HttpStream)
                           (implicit execctx: ExecutionContext) extends ChannelHandler {

  private var stream: Option[HttpStream] = None

  private def getStream(ctx: ChannelHandlerContext, req: HttpReq) = {
    stream.getOrElse {
      val q = queryFactory(ctx, req)
      stream = Some(q)
      q
    }
  }

  def handle(ctx: ChannelHandlerContext, req: HttpReq) {
    val stream = getStream(ctx, req)

    stream.startRequest(req)
    try {
      handler.handle(req).onComplete {
        case Success(resp) =>
          stream.sendResponse(req.id, resp)

        case Failure(e) =>
          log.warn("Unexpected Http error", e)
          stream.sendResponse(req.id, HttpResp(status = HttpResponseStatus.INTERNAL_SERVER_ERROR))
      }
    } catch {
      case x: Exception =>
        log.warn("Unexpected Http.handle error", x)
        stream.sendResponse(req.id, HttpResp(status = HttpResponseStatus.INTERNAL_SERVER_ERROR))
    }
  }

  override def channelInactive(ctx: ChannelHandlerContext) {
    super.channelInactive(ctx)
    stream.foreach(_.stop())
  }
}
