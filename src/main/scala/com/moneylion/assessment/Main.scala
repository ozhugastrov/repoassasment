package com.moneylion.assessment

import scala.io.StdIn.readLine

object Main extends App {
  private val boundingBox = new BoundingBox
  private val lines = LazyList.continually(readLine).takeWhile(_.nonEmpty).map(_.toCharArray).toArray
  boundingBox.findLargestBoxes(lines).foreach(println)
}


