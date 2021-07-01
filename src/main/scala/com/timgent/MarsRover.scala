package com.timgent

import com.timgent.RoverResult.{OutOfBoundsCoords, OutOfBoundsRoverPosition}
import enumeratum.{Enum, EnumEntry}

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
object MarsRover extends App {
  println("""
      |======================
      |Welcome to Mars Rover!
      |======================
      |Now let's get roving! Please enter details of your map grid and rovers as follows:
      |
      |- The first entry should be the size of your grid in the format x y. e.g. 5 4
      |
      |- Subsequent entries each represent a rover and their movement in the format (x, y, D) <movement>. e.g. (1, 3, N) FLLFRF
      |  - x and y represent the starting co-ordinates for the rover
      |  - D represents the direction the rover is facing and can be N for North, E for East, S for South, or W for West
      |  - <movement> is a list of instructions, options are F for forward, L for left, R for right
      |  
      |Enter a blank line when you have finished providing input
      |""".stripMargin)
  val result = run(getInput = () => Option(readLine()))
  println("Nice job! This is where your rovers ended up:")
  println(result)

  def run(getInput: () => Option[String]): String = {
    val result = for {
      roverSetup <- readRoverInstructions(getInput)
      roverResults = interpretInstructions(roverSetup)
    } yield roverResults

    result match {
      case Right(roverResults) => roverResults.map(_.prettyPrint).mkString("\n")
      case Left(err)           => err.userErr
    }
  }

  def interpretInstructions(roverSetup: RoverSetup): List[RoverResult] = {
    roverSetup.rovers.map { rover =>
      val updatedRover: Either[OutOfBoundsRoverPosition, RoverPosition] = {
        rover.instructions.foldLeft[Either[OutOfBoundsRoverPosition, RoverPosition]](Right(rover.position)) {
          (currPosition, currentInstruction) =>
            currPosition match {
              case Left(outOfBoundsPos)      => Left(outOfBoundsPos)
              case Right(validRoverPosition) => validRoverPosition.move(currentInstruction, roverSetup.mapSize)
            }
        }
      }
      RoverResult(updatedRover)
    }
  }

  def readRoverInstructions(getInput: () => Option[String]): Either[RoverParseErr, RoverSetup] = {
    @tailrec
    def readRoverInstructions(inputSoFar: Option[RoverSetup]): Either[RoverParseErr, RoverSetup] = {
      val inputLine = getInput()
      inputLine match {
        case Some(newInputStr) if newInputStr != "" =>
          inputSoFar match {
            case Some(roverSetup) =>
              val maybeParsedRover = parseRover(newInputStr, roverSetup.mapSize)
              maybeParsedRover match {
                case Right(rover) => readRoverInstructions(Some(roverSetup.copy(rovers = roverSetup.rovers :+ rover)))
                case Left(err)    => Left(err)
              }
            case None =>
              val maybeParsedMapSize = parseMapSize(newInputStr)
              maybeParsedMapSize match {
                case Right(mapSize) => readRoverInstructions(Some(RoverSetup(mapSize, List.empty)))
                case Left(err)      => Left(err)
              }
          }
        case _ =>
          inputSoFar match {
            case Some(roverSetup) if roverSetup.rovers.isEmpty => Left(NoRovers)
            case Some(roverSetup)                              => Right(roverSetup)
            case None                                          => Left(NoInputAtAll)
          }
      }
    }

    readRoverInstructions(None)
  }

  def parseRover(str: String, mapSize: MapSize): Either[BadRoverPosWithInstructions, RoverPosWithInstructions] = {
    val regex: Regex = "\\((-?\\d+), (-?\\d+), (\\w)\\) (\\w+)".r()
    val matches = regex.findAllIn(str).matchData.toList
    matches match {
      case singleMatch :: Nil =>
        val maybeRover = for {
          xCoordinate <- Try(singleMatch.group(1).toInt)
          yCoordinate <- Try(singleMatch.group(2).toInt)
          maybeCoordinates = Coordinates(xCoordinate, yCoordinate).validateCoords(mapSize)
          direction <- Try(singleMatch.group(3)).map(char => Direction.withName(char.toString))
          instructionsList <- Try(singleMatch.group(4).map(char => Instruction.withName(char.toString)).toList)
          roverOrOutOfBounds = maybeCoordinates match {
            case Left(outOfBoundsCoords) => Left(OutOfBoundsRover)
            case Right(validCoordinates) => Right(RoverPosWithInstructions(RoverPosition(validCoordinates, direction), instructionsList))
          }
        } yield roverOrOutOfBounds
        maybeRover match {
          case Failure(_)          => Left(InvalidRoverDetails)
          case Success(maybeRover) => maybeRover
        }
      case _ => Left(InvalidRoverDetails)
    }
  }

  def parseMapSize(str: String): Either[BadMapSize.type, MapSize] = {
    val rawCoords = str.split(" ")
    if (rawCoords.length != 2)
      Left(BadMapSize)
    else {
      val maybeMapSize = for {
        height <- Try(rawCoords(0).toInt).toOption
        width <- Try(rawCoords(1).toInt).toOption
      } yield MapSize(height, width)
      maybeMapSize match {
        case Some(mapSize) if mapSize.width < 0 || mapSize.height < 0 => Left(BadMapSize)
        case Some(mapSize)                                            => Right(mapSize)
        case None                                                     => Left(BadMapSize)
      }
    }
  }

}

sealed trait RoverParseErr {
  def userErr: String
}
sealed trait BadRoverPosWithInstructions extends RoverParseErr
case object InvalidRoverDetails extends BadRoverPosWithInstructions {
  override def userErr: String = "Could not parse the rover details. Please try again"
}
case object OutOfBoundsRover extends BadRoverPosWithInstructions {
  override def userErr: String = "The specified rover is out of bounds of the given map. Please try again"
}
case object BadMapSize extends RoverParseErr {
  override def userErr: String = "Could not parse the map size. Please try again"
}
case object NoRovers extends RoverParseErr {
  override def userErr: String = "No rover data was entered. Please try again"
}
case object NoInputAtAll extends RoverParseErr {
  override def userErr: String = "No map or rover data was entered. Please try again"
}

case class RoverSetup(mapSize: MapSize, rovers: List[RoverPosWithInstructions])
case class RoverPosWithInstructions(position: RoverPosition, instructions: List[Instruction])
case class RoverPosition(position: Coordinates, direction: Direction) {
  def prettyPrint = s"(${position.x}, ${position.y}, ${direction.entryName})"

  def move(instruction: Instruction, mapSize: MapSize): Either[OutOfBoundsRoverPosition, RoverPosition] =
    instruction match {
      case Instruction.Forward   => position.move(direction, mapSize)
      case Instruction.TurnLeft  => Right(RoverPosition(position, direction.turnLeft))
      case Instruction.TurnRight => Right(RoverPosition(position, direction.turnRight))
    }
}
sealed abstract class Instruction(override val entryName: String) extends EnumEntry
object Instruction extends Enum[Instruction] {
  val values = findValues
  case object Forward extends Instruction("F")
  case object TurnLeft extends Instruction("L")
  case object TurnRight extends Instruction("R")
}
sealed abstract class Direction(override val entryName: String) extends EnumEntry {
  def turnRight: Direction
  def turnLeft: Direction
}
object Direction extends Enum[Direction] {
  val values = findValues
  case object North extends Direction("N") {
    def turnRight: Direction = East
    def turnLeft: Direction = West
  }
  case object South extends Direction("S") {
    def turnRight: Direction = West
    def turnLeft: Direction = East
  }
  case object East extends Direction("E") {
    def turnRight: Direction = South
    def turnLeft: Direction = North
  }
  case object West extends Direction("W") {
    def turnRight: Direction = North
    def turnLeft: Direction = South
  }
}
case class Coordinates(x: Int, y: Int) {
  def validateCoords(mapSize: MapSize): Either[OutOfBoundsCoords, Coordinates] = {
    // TODO: Double check what constitutes valid co-ordinates
    // TODO: This method could return just a boolean
    if (x < 0 || y < 0 || x > mapSize.width || y > mapSize.height)
      Left(this)
    else
      Right(this)
  }
  def move(direction: Direction, mapSize: MapSize): Either[OutOfBoundsRoverPosition, RoverPosition] = {
    val newCoords = direction match {
      case Direction.North => Coordinates(x, y + 1)
      case Direction.South => Coordinates(x, y - 1)
      case Direction.East  => Coordinates(x + 1, y)
      case Direction.West  => Coordinates(x - 1, y)
    }
    val validatedCoords = newCoords.validateCoords(mapSize)
    validatedCoords match {
      case Left(outOfBoundsCoords) => Left(RoverPosition(this, direction))
      case Right(goodCoordinates)  => Right(RoverPosition(goodCoordinates, direction))
    }
  }
}
case class MapSize(height: Int, width: Int)
case class RoverResult(roverPosition: Either[OutOfBoundsRoverPosition, RoverPosition]) {
  def prettyPrint: String =
    roverPosition match {
      case Left(outOfBoundsRoverPosition) => s"${outOfBoundsRoverPosition.prettyPrint} LOST"
      case Right(roverPosition)           => roverPosition.prettyPrint
    }
}

object RoverResult {
  type OutOfBoundsRoverPosition = RoverPosition
  type OutOfBoundsCoords = Coordinates
}
