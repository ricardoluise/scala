package org.ricardo.mt.http.matching

import io.netty.handler.codec.http.HttpMethod
import org.ricardo.mt.http._

object -> {
    def unapply(req: HttpReq): Option[(HttpMethod, String)] = Some(req.method -> req.path)
  }

  object /: {
    def unapply(p: String): Option[(String, String)] = p.split("/").toList match {
        case x :: y => Some(x, y.mkString("/"))
        case _ => None
    }
  }

  object / {
    def unapply(s: String): Option[(String, String)] = {
      val slash = s.lastIndexOf("/")
      if (slash > 0 && slash < s.length - 1) {
        Some(s.substring(0, slash), s.substring(slash + 1))
      } else {
        None
      }
    }
  }

  object :? {
    def unapply(req: HttpReq): Option[(HttpReq, MultiStringMap)] = Some(req, req.query)
  }
  object & {
    def unapply(req: HttpReq): Option[(HttpReq, MultiStringMap)] = Some(req, req.query)
  }

  object Test {
    def main (args: Array[String]) {
      "a/b/c/d" match {
        case "a/b/c" / x => println(x)
        case _ => println("not matched")
      }
    }
  }
