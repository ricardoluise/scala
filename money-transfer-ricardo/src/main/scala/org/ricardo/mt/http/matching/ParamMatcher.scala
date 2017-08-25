package org.ricardo.mt.http.matching

import org.ricardo.mt.http._

import scala.util.Try


trait ParamMatcher[A] {
  def unapply(params: MultiStringMap): Option[A]
}

class ScalarParam[A](name: String)(implicit parser: ScalarParser[A]) extends ParamMatcher[A] {
  def unapply(params: MultiStringMap): Option[A] = params.getOne(name).flatMap(parser.unapply)
}

trait ScalarParser[A] {
  def unapply(s: String): Option[A]
}

object ScalarParser {
  implicit object str extends ScalarParser[String] {
    def unapply(s: String): Option[String] = Some(s)
  }

  implicit object int extends ScalarParser[Int] {
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption
  }
}
