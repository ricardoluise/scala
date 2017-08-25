package org.ricardo.mt.http

package object path {

  object Parameters {
    def empty: Parameters = Map.empty
  }
  type Parameters = Map[String, Any]


  implicit def stringToSegment(s: String): Segment = StaticSegment(s)
  implicit def stringToPath(s: String): Path = Path() / StaticSegment(s)
}
