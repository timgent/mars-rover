package com.timgent

import com.timgent.Direction.{East, South}
import com.timgent.Instruction.{Forward, TurnLeft, TurnRight}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class MarsRoverSpec extends AnyWordSpec with Matchers {
  def runMarsRoverWithCannedInput(input: List[String]) = {
    val iterator = input.toIterator
    MarsRover.run(() => Try(iterator.next()).toOption)
  }
  "MarsRover.run" should {
    "correctly report the final positions of multiple rovers" in {
      val input = List(
        "4 8",
        "(2, 3, E) LFRFF",
        "(0, 2, N) FFLFRFF"
      )
      val result = runMarsRoverWithCannedInput(input)
      val expected =
        """(4, 4, E)
          |(0, 4, W) LOST""".stripMargin
      result shouldBe expected
    }

    "give the user an appropriate error when no input is given" in {
      runMarsRoverWithCannedInput(List("")) shouldBe "No map or rover data was entered. Please try again"
    }

    "give the user an appropriate error when no rover data is given" in {
      runMarsRoverWithCannedInput(
        List(
          "4 8"
        )
      ) shouldBe "No rover data was entered. Please try again"
    }

    "give the user an appropriate error when the rover details are not valid" in {
      val badInput = List("4 8", "invalid input")
      val result = runMarsRoverWithCannedInput(badInput)
      result shouldBe "Could not parse the rover details. Please try again"
    }

    "give the user an appropriate error when the map size input is not valid" in {
      val badInput = List("invalid input")
      val result = runMarsRoverWithCannedInput(badInput)
      result shouldBe "Could not parse the map size. Please try again"
    }
  }

  "MarsRover.parseMapSize" should {
    "fail to parse a map size with a negative number" in {
      MarsRover.parseMapSize("-1 5") shouldBe Left(BadMapSize)
    }
  }

  "MarsRover.parseRover" should {
    "successfully parse input" when {
      "given a valid input string" in {
        MarsRover.parseRover("(2, 3, E) LFRFF", MapSize(4, 4)) shouldBe Right(
          RoverPosWithInstructions(
            RoverPosition(Coordinates(2, 3), East),
            List(
              TurnLeft,
              Forward,
              TurnRight,
              Forward,
              Forward
            )
          )
        )
      }
    }

    "fail to parse input" when {
      "the rover is out of bounds of the map" in {
        MarsRover.parseRover("(2, 3, N) LFR", MapSize(1, 1)) shouldBe Left(OutOfBoundsRover)
      }
      "the rover has a negative co-ordinate" in {
        MarsRover.parseRover("(2, -1, N) LFR", MapSize(4, 4)) shouldBe Left(OutOfBoundsRover)
      }
      "the overall format is incorrect" in {
        MarsRover.parseRover("(2, 3) LFR", MapSize(4, 4)) shouldBe Left(InvalidRoverDetails)
      }
      "one of the coordinates is not a number" in {
        MarsRover.parseRover("(2, x, E) LFRFF", MapSize(4, 4)) shouldBe Left(InvalidRoverDetails)
        MarsRover.parseRover("(y, 3, E) LFRFF", MapSize(4, 4)) shouldBe Left(InvalidRoverDetails)
      }
      "the direction of the rover is not valid" in {
        MarsRover.parseRover("(y, 3, P) LFRFF", MapSize(4, 4)) shouldBe Left(InvalidRoverDetails)
      }
      "any of the instructions is not valid" in {
        MarsRover.parseRover("(y, 3, E) LFRFB", MapSize(4, 4)) shouldBe Left(InvalidRoverDetails)
      }
    }
  }

  "MarsRover.interpretInstructions" should {
    "identify the first time a rover goes out of bounds" in {
      MarsRover.interpretInstructions(
        RoverSetup(
          MapSize(2, 1),
          List(
            RoverPosWithInstructions(RoverPosition(Coordinates(0, 0), South), List(Forward, TurnLeft, TurnLeft, Forward, Forward, Forward))
          )
        )
      ) shouldBe List(RoverResult(Left(RoverPosition(Coordinates(0, 0), South))))
    }
  }
}
