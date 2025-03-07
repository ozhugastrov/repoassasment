package com.moneylion.assessment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BoundingBoxSpec extends AnyFlatSpec with should.Matchers {
  private def parseInput(input: String): Array[Array[Char]] = {
    input.split("\n").toList.map(_.toCharArray).toArray
  }

  val boundingBox = new BoundingBox

  "Empty input" should "be empty" in {
    boundingBox.findLargestBoxes(Array.empty) should be(List.empty[BoundingBox#Box])
  }


  "First test example" should "be (2,2)(3,3)" in {
    val input =
      """----
        |-**-
        |-**-
        |----
        |""".stripMargin

    boundingBox.findLargestBoxes(parseInput(input)).map(_.toString) should be(List("(2,2)(3,3)"))
  }


  "Second test example" should "be (1,1)(2,2)" in {
    val input =
      """**-------***
        |-*--**--***-
        |-----***--**
        |-------***--
        |""".stripMargin

    boundingBox.findLargestBoxes(parseInput(input)).map(_.toString) should be(List("(1,1)(2,2)"))
  }

  "Test no asterisks" should "be empty" in {
    val input =
      """----
        |----
        |----
        |----
        |""".stripMargin
    boundingBox.findLargestBoxes(parseInput(input)) should be(List.empty[BoundingBox#Box])
  }

  "With inner block" should "be (1,12)(3,12)" in {
    val input =
      """--********-*
        |--*------*-*
        |--*---*--*-*
        |--*---*--*--
        |--*------*-*
        |--********-*
        |""".stripMargin

    boundingBox.findLargestBoxes(parseInput(input)).map(_.toString) should be(List("(1,12)(3,12)"))

  }

  "With same square" should "be (2,2)(2,2) (4,4)(4,4)" in {
    val input =
      """-
        |*
        |-
        |*
        |""".stripMargin

    boundingBox.findLargestBoxes(parseInput(input)).map(_.toString) should be(List("(2,1)(2,1)", "(4,1)(4,1)"))
  }

  "Entire grid" should "be (1,1)(3,3)" in {
    val input =
      """***
        |***
        |***
        |""".stripMargin

    boundingBox.findLargestBoxes(parseInput(input)).map(_.toString) should be(List("(1,1)(3,3)"))
  }

  "Two boxes" should "be (1,1)(2,2) (3,3)(4,4)" in {
    val input =
      """**--
        |**--
        |--**
        |--**
        |""".stripMargin

    boundingBox.findLargestBoxes(parseInput(input)).map(_.toString).sorted should be(List("(1,1)(2,2)", "(3,3)(4,4)"))
  }

}
