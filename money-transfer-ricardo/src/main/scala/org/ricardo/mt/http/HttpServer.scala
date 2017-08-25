package org.ricardo.mt.http

import java.util.concurrent.Executors

import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.PooledByteBufAllocator
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http._
import org.ricardo.mt.util.Loggable

import scala.concurrent.ExecutionContext


class HttpServer(handler: RequestHandler,
                 val options: HttpServerOptions = HttpServerOptions()) extends Loggable {

  private val bossGroup = new NioEventLoopGroup(1)
  private val workerGroup = new NioEventLoopGroup(Runtime.getRuntime.availableProcessors() * 2 + 1)

  def shutdown(): Unit = {
    log.info("--> Stopping HttpServer")
    bossGroup.shutdownGracefully()
    workerGroup.shutdownGracefully()
    log.info("-- Stopping (await) HttpServer")
    bossGroup.terminationFuture().sync()
    workerGroup.terminationFuture().sync()

    log.info("<-- Stopped HttpServer")
  }

  def start(): Unit = {
    val boot = new ServerBootstrap()
    boot.group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new HttpServerChannelInitializer(handler, options)(ExecutionContext.fromExecutorService(workerGroup)))
      .option(ChannelOption.SO_BACKLOG, new Integer(16000))

    boot.bind(options.port).sync()
    log.info(s"HTTP server started with options: $options")
  }

}

class HttpServerChannelInitializer(handler: RequestHandler, options: HttpServerOptions)(implicit execctx: ExecutionContext)
    extends ChannelInitializer[SocketChannel] {

  private val scheduler = Executors.newScheduledThreadPool(4)
  private val allocator = new PooledByteBufAllocator(true)

  override def initChannel(ch: SocketChannel) = {

    ch.config().setAllocator( allocator )

    val p = ch.pipeline()
    p.addLast("httpDecoder",    new HttpRequestDecoder(options.maxInitialLineLength, options.maxHeaderSize, options.maxChunkSize))
    p.addLast("httpEncoder",    new HttpResponseEncoder())
    p.addLast("httpCompressor", new HttpContentCompressor())
    p.addLast("httpAggregator", new HttpObjectAggregator(options.maxContentLength))
    p.addLast("strdDecoder",    new StrdRequestDecoder())
    p.addLast("strdEncoder",    new StrdResponseEncoder())
    p.addLast("handler",        new DefaultChannelHandler(handler, HttpStream.factory(scheduler, options.timeout)))
  }
}

case class HttpServerOptions(port                 : Int = 80,
                             maxChunkSize         : Int = 8192,
                             maxInitialLineLength : Int = 4096,
                             maxHeaderSize        : Int = 8192,
                             maxContentLength     : Int = 10240,
                             timeout              : Int = 30000) {

  override def toString = getClass
    .getDeclaredFields.map(_.getName) // all field names
    .zip(productIterator.to)
    .map(x => s"${x._1} = ${x._2}").mkString(", ")
}
