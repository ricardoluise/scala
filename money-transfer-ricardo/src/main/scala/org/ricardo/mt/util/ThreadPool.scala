package org.ricardo.mt.util

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext


object ThreadPool {
  def fixedPool(threads: Int = Runtime.getRuntime.availableProcessors()): ExecutionContext = {
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threads))
  }
}
