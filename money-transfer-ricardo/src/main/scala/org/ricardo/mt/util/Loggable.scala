package org.ricardo.mt.util

import com.typesafe.scalalogging.Logger


trait Loggable {
  implicit protected val log = Logger(getClass)
}
