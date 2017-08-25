package org.ricardo.mt.http

import io.netty.buffer.{Unpooled, ByteBuf}
import io.netty.util.CharsetUtil


object Content {
  def apply(str: String): ByteBuf = {
    Unpooled.wrappedBuffer(str.getBytes(CharsetUtil.UTF_8))
  }

  def apply(bytes : Array[Byte]): ByteBuf = {
    Unpooled.wrappedBuffer(bytes)
  }
}
