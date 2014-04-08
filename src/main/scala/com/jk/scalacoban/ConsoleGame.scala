package com.jk.scalacoban

trait ConsoleGame extends Game {
  def drawTerrain = {
    val Position(bx, by) = terrain.bounds
    def boxPositions = boxes map { e: Entity => e.position }

    def drawStrip(y: Int) {
      for { x <- 0 to bx } {
        if (user.position == Position(x, y)) print("*")
        else if (boxPositions.contains(Position(x, y))) print("@")
        else if (terrain.isWall(Position(x, y))) print("#")
        else if (terrain.isSink(Position(x, y))) print("o")
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

  def handleInput = {
    Console.readLine match {
      case "u" => moveUser(MoveUp)
      case "l" => moveUser(MoveLeft)
      case "r" => moveUser(MoveRight)
      case "d" => moveUser(MoveDown)
      case _ =>
    }
  }

  def run() {
    print("Type 'u', 'l', 'r' or 'd' and press enter to move.\n")
    while (!isFinished) {
      drawTerrain
      handleInput
    }
    drawTerrain
    print("Done!")
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

object ConsoleGameImpl extends ConsoleGame {
  val user = new Entity(0, 0)
  val boxes = Set(new Entity(1, 1), new Entity(2, 2))
  val terrain = SimpleTerrain

  def main(args: Array[String]) {
    run()
  }
}