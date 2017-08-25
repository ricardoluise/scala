package org.ricardo.mt

import io.netty.buffer.ByteBuf
import io.netty.handler.codec.http.cookie.{Cookie => NettyCookie}

import scala.language.implicitConversions


package object http extends MultiStringMapUtils
                            with HttpMethodAliases
                            with HeaderUtils {


  type MultiStringMap = Map[String, Seq[String]]
  type MultiString =  (String, Seq[String])
  type Cookie = NettyCookie

  implicit def stringToContent(str:String) : Option[ByteBuf] = Some(Content(str))
}
