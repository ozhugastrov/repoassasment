package com.moneylion.assessment

import com.moneylion.assessment.Main.lines

import scala.annotation.tailrec

class BoundingBox {
  final case class Point(x: Int, y: Int)

  final case class Box(topLeft: Point, btmRight: Point) {
    def sqr: Int = Math.max(1, btmRight.y - topLeft.y) * Math.max(1, btmRight.x - topLeft.x)

    override def toString: String = s"(${topLeft.y + 1},${topLeft.x + 1})(${btmRight.y + 1},${btmRight.x + 1})"
  }

  @tailrec
  private def getBox(topLeft: Point, btmRight: Point, visited: Set[Point], toVisit: List[Point], matrix: Array[Array[Char]]): Box = {
    if (toVisit.isEmpty)
      Box(topLeft, btmRight)
    else if (isAsterisk(toVisit.head, matrix)) {
      val current = toVisit.head
      matrix(current.y)(current.x) = '-' // mark visited
      val neighbors =
        Point(current.x, current.y + 1) ::
          Point(current.x, current.y - 1) ::
          Point(current.x + 1, current.y) ::
          Point(current.x - 1, current.y) ::
          toVisit.tail // get all neighbors of current point
      val nextToVisit = neighbors.filterNot(visited.contains) // remove points we are already been at
      val nextTopLeft = Point(Math.min(current.x, topLeft.x), Math.min(current.y, topLeft.y)) // check if we moved further top left
      val nextBtmRight = Point(Math.max(current.x, btmRight.x), Math.max(current.y, btmRight.y)) // check if we moved further btm down
      val nextVisited = visited + current
      getBox(nextTopLeft, nextBtmRight, nextVisited, nextToVisit, matrix)
    }
    else {
      getBox(topLeft, btmRight, visited + toVisit.head, toVisit.tail, matrix)
    }
  }

  private def isAsterisk(point: Point, matrix: Array[Array[Char]]) = {
    matrix.isDefinedAt(point.y) && matrix(point.y).isDefinedAt(point.x) && matrix(point.y)(point.x) == '*'
  }

  private def areOverlapping(first: Box)(second: Box): Boolean = {
    // Check if one box is completely to the left/right/above/below the other
    !(first.btmRight.x < second.topLeft.x ||
      first.topLeft.x > second.btmRight.x ||
      first.btmRight.y < second.topLeft.y ||
      first.topLeft.y > second.btmRight.y)
  }

  private def areOverlapping1(first: Box)(second: Box) = {
    val secondXRange = second.topLeft.x to second.btmRight.x
    val secondYRange = second.topLeft.y to second.btmRight.y
    ((secondXRange contains first.topLeft.x) && (secondYRange contains first.btmRight.y)) || // checking bottom left corner
      ((secondXRange contains first.btmRight.x) && (secondYRange contains first.btmRight.y)) || // checking bottom right corner
      ((secondXRange contains first.topLeft.x) && (secondYRange contains first.topLeft.y)) || // checking top left corner
      ((secondXRange contains first.btmRight.x) && (secondYRange contains first.topLeft.y)) // checking top right corner
  }

  def findLargestBoxes(matrix: Array[Array[Char]]): List[Box] = {
    // get all possible boxes from input matrix
    val boxes = for {i <- matrix.indices
                     j <- matrix.head.indices
                     if matrix(i)(j) == '*'} yield {
      getBox(Point(j, i), Point(j, i), Set.empty, List(Point(j, i)), matrix)
    }

    val (nonOverlapping, _) = boxes.foldLeft((Set.empty[Box], Set.empty[Box])) { case ((nonOverlap, overlap), b) =>
      if (overlap.contains(b)) (nonOverlap, overlap) // if current box already in overlapped skip it
      else {
        nonOverlap.filter(areOverlapping(b)).toList match { //check if current box overlapping with other boxes
          case Nil => (nonOverlap + b, overlap) // if not add it
          case a => (nonOverlap -- a, overlap ++ a + b) // if it is overlapping with other boxes remove them from non overlapped and add to overlapped
        }
      }
    }

    nonOverlapping.groupBy(_.sqr).toList.sortBy(_._1).reverse.headOption.toList.flatMap(_._2)
  }
}
