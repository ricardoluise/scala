package org.ricardo.mt.http.path


sealed trait Segment {
  def string: String

  def parse(s: String): Option[Parameters]

  override def toString: String = string

}

case class StaticSegment(string: String) extends Segment {
  def parse(s: String) = s match {
    case `string` => Some(Parameters.empty)
    case _ => None
  }
}

abstract class PathParameter[T] extends Segment {
  def name: String
  def string = s"{$name}"

  def parseType(s: String): Option[T]

  def parse(s: String) = parseType(s).map(p => Map(name -> p))
}

case class int(name: String) extends PathParameter[Int] {
  override def parseType(s: String) = try {
    Some(s.toInt)
  } catch {
    case _: Exception => None
  }
}

case class long(name: String) extends PathParameter[Long] {
  override def parseType(s: String) = try {
    Some(s.toLong)
  } catch {
    case _: Exception => None
  }
}

case class string(name: String) extends PathParameter[String] {
  override def parseType(s: String) = Some(s)
}

case class enum(name: String, enum: Enumeration) extends PathParameter[Enumeration#Value] {
  override def parseType(s: String) = try {
    Some(enum.withName(s))
  } catch {
    case _: Exception => None
  }
}
