package org.ricardo.mt.http

import io.netty.handler.codec.http.HttpMethod


trait HttpMethodAliases {
  val DELETE  = HttpMethod.DELETE
  val GET     = HttpMethod.GET
  val HEAD    = HttpMethod.HEAD
  val OPTIONS = HttpMethod.OPTIONS
  val PATCH   = HttpMethod.PATCH
  val POST    = HttpMethod.POST
  val PUT     = HttpMethod.PUT
}
