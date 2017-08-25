package org.ricardo.mt.http

import io.netty.handler.codec.http.HttpHeaders
import io.netty.handler.codec.http.cookie.{DefaultCookie, ServerCookieEncoder, Cookie => NettyCookie}

import scala.collection.JavaConversions._


object Cookie {
  def apply(name: String, value: String, path: String = "/", domain: Option[String] = None, maxAge: Option[Long] = None): NettyCookie = {
    val cookie = new DefaultCookie(name, value)
    cookie.setPath(path)
    domain.foreach(cookie.setDomain)
    maxAge.foreach(cookie.setMaxAge)

    cookie
  }
  def getHeader(cookie: NettyCookie) = HttpHeaders.Names.SET_COOKIE -> Seq(ServerCookieEncoder.STRICT.encode(cookie))
  def getHeader(cookies: Seq[NettyCookie]): (String, Seq[String]) = HttpHeaders.Names.SET_COOKIE -> ServerCookieEncoder.STRICT.encode(cookies)
}
