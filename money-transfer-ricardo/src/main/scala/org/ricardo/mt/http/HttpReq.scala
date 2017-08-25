package org.ricardo.mt.http

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.charset.Charset

import io.netty.handler.codec.http.cookie.ServerCookieDecoder
import io.netty.handler.codec.http.{HttpHeaders, HttpMethod, QueryStringDecoder}

import scala.collection.convert.decorateAsScala._
import scala.util.Try


trait HttpReq {
  def id: Long
  def started: Long
  def method: HttpMethod
  def uri: String
  def headers: MultiStringMap
  def content: ByteBuffer
  def keepAlive: Boolean

  def ip: String

  lazy val (path, query) = {
    val decoder = new QueryStringDecoder(uri)
    (decoder.path().dropWhile(_ == '/'), MultiStringMap(decoder.parameters()))
  }

  lazy val cookies = {
    headers.get(HttpHeaders.Names.COOKIE).map { values =>
      values.flatMap { c =>
        Try(ServerCookieDecoder.STRICT.decode(c).asScala).getOrElse(Set.empty[Cookie])
      }.map { c =>
        c.name() -> c.value()
      }.toMap
    }.getOrElse(Map.empty)
  }

  def processingTime = System.currentTimeMillis() - started


  lazy private val _params = new QueryStringDecoder(contentString, false).parameters()

  lazy val contentBytes =  {
    val b = new Array[Byte](content.limit())
    content.get(b)
    b
  }
  def contentString = new String(contentBytes, Charset.forName("UTF-8"))
  def contentParams = _params
  def contentStream = new ByteArrayInputStream(contentBytes) // TODO: use some native byte buf input stream


  def host      : Option[String] = headers.getOne(HttpHeaders.Names.HOST)
  def userAgent : Option[String] = headers.getOne(HttpHeaders.Names.USER_AGENT)
  def referrer  : Option[String] = headers.getOne(HttpHeaders.Names.REFERER)


  override def toString =
    s"""
      |HttpReq: $id $started $ip
      |$method $uri
      |${headers.toSeq.flatMap(h => h._2.map(v => s"${h._1}: $v")).mkString("\n")}
      |
      |${content.limit()} bytes
    """.stripMargin.trim
}

