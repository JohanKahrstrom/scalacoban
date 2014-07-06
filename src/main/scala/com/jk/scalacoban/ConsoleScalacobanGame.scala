package com.jk.scalacoban

import com.jk.game.Game

trait ConsoleScalacobanGame extends Game[Move, ScalacobanGame] {
  def draw(state: ScalacobanGame) = {
    val Position(bx, by) = state.terrain.bounds
    def boxPositions = state.boxes map { e: Entity => e.position }

    def drawStrip(y: Int) {
      for { x <- 0 to bx } {
        if (state.user.position == Position(x, y)) print("*")
        else if (boxPositions.contains(Position(x, y))) print("@")
        else if (state.terrain.isWall(Position(x, y))) print("#")
        else if (state.terrain.isSink(Position(x, y))) print("o")
        else print(" ")
      }
    }

    for { x <- 0 to (bx + 2) } { print("-") }
    print("\n")
    for { y <- 0 to by } {
      print("|")
      drawStrip(y)
      print("|\n")
    }
    for { x <- 0 to (bx + 2) } { print("-") }
    print("\n")
  }

  def readInput: Move = {
    Console.readLine match {
      case "u" => MoveUp
      case "l" => MoveLeft
      case "r" => MoveRight
      case "d" => MoveDown
      case _ => NoMove
    }
  }

  def update(input: => Move, state: ScalacobanGame): ScalacobanGame = {
    if (state.isFirst) {
      state.notFirst
    } else {
      state.moveUser(input)
    }
  }

  def shouldQuit(state: ScalacobanGame): Boolean = {
    state.isFinished
  }
}

object SimpleTerrain extends Level {
  def isFloor(position: Position) = (position, bounds) match {
    case (Position(2, 4), _) => false
    case (Position(3, 4), _) => false
    case (Position(x, y), Position(bx, by)) if x >= 0 && x <= bx && y >= 0 && y <= bx => true
    case _ => false
  }
  def isSink(pos: Position) = pos == Position(3, 2) || pos == Position(3, 3)
  def bounds = Position(5, 5)
}

object ConsoleScalacobanGameImpl extends ConsoleScalacobanGame {
  val user = new Entity(0, 0)
  val boxes = Set(new Entity(1, 1), new Entity(2, 2))
  val terrain = SimpleTerrain
  val scalacobanGame = ScalacobanGame(user, boxes, terrain, true)

  def main(args: Array[String]) {
    run(scalacobanGame)
  }
}