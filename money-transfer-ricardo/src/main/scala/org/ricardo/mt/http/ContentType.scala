package org.ricardo.mt.http

import io.netty.handler.codec.http.HttpHeaders


object ContentType {
  val JS      = "application/javascript;charset=utf-8" // charset is recommended
  val HTML    = "text/html;charset=utf-8" // charset is recommended
  val JSON    = "application/json"
  val XML     = "text/xml"
  val PLAIN   = "text/plain"
  val GIF     = "image/gif"
  val BINARY  = "application/octet-stream"
  val CSS     = "text/css"
  val PNG     = "image/png"
  val ICO     = "image/x-icon"
  val TTF     = "application/x-font-ttf"

  def apply(x: (this.type ) => String): (String, Seq[String]) = HttpHeaders.Names.CONTENT_TYPE -> Seq(x(this))
  def apply(s: String): (String, Seq[String]) = HttpHeaders.Names.CONTENT_TYPE -> Seq(s)
}
