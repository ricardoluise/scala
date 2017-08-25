package org.ricardo.mt.http.path


object Path {
  def apply(segments: Segment*): Path = apply(segments.toList)
}

case class Path(segments: List[Segment]) {

  def parse(s: String): Option[Parameters] = {
    val pathSegments = s.stripPrefix("/").stripSuffix("/").split("/").filter(!_.isEmpty)
    if (pathSegments.length != segments.size) {
      None
    } else {
      segments.zip(pathSegments).foldLeft[Option[Parameters]](Some(Parameters.empty))((r, s) => r match {
        case None => None
        case Some(p) => s._1.parse(s._2).map(p ++ _)
      })
    }
  }

  override def toString: String = "/" + segments.mkString("/")

  def /(s: Segment) = Path(segments :+ s)

  def +(p: Path) = Path(segments ++ p.segments)

  def base(p: Path) = Path(segments.zip(p.segments).takeWhile(Function.tupled(_ == _)).map(_._1))

  def staticPrefix = Path(segments.takeWhile(_.isInstanceOf[StaticSegment]))

  def parameters = segments.collect {
    case p: PathParameter[_] => p
  }

  def isRoot = segments.isEmpty
}
