package org.ricardo.mt.http

import io.netty.buffer.ByteBuf
import io.netty.handler.codec.http.HttpHeaders.Names._
import io.netty.handler.codec.http.HttpResponseStatus
import io.netty.handler.codec.http.HttpResponseStatus._


case class HttpResp(status      : HttpResponseStatus = OK,
                    body        : Option[ByteBuf] = None,
                    headers     : MultiStringMap = Map.empty) {

  def +(kv: (String, Seq[String])) = copy(headers = headers + kv)
  def :+(kv: (String, String)) = copy(headers = headers :+ kv)
}

object HttpResp {
  val EMPTY       = HttpResp(NO_CONTENT)
  val BAD_REQ     = HttpResp(BAD_REQUEST)
  val NOT_FOUND   = HttpResp(HttpResponseStatus.NOT_FOUND)


  def redirect(url: String, headers : MultiStringMap = Map.empty) = HttpResp(
    status = FOUND,
    headers = headers :+ (LOCATION -> url)
  )

}
