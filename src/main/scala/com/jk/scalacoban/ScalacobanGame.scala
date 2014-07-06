package com.jk.scalacoban

import scala.language.postfixOps

case class Position(x: Int, y: Int) {
  def right = Position(x + 1, y)
  def left  = Position(x - 1, y)
  def up    = Position(x, y - 1)
  def down  = Position(x, y + 1)
}

sealed trait Move { def move(position: Position): Position }
case object NoMove extends Move { def move(position: Position) = position }
case object MoveLeft extends Move { def move(position: Position) = position.left }
case object MoveRight extends Move { def move(position: Position) = position.right }
case object MoveUp extends Move { def move(position: Position) = position.up }
case object MoveDown extends Move { def move(position: Position) = position.down }

case class Entity(val position: Position) {
  def this(x: Int, y: Int) = this(Position(x, y))

  def right = position.right
  def left  = position.left
  def up    = position.up
  def down  = position.down

  def move(m: Move): Entity = { Entity(m.move(position)) }
}

trait Level {
  def isFloor(pos: Position): Boolean
  def isSink(pos: Position): Boolean
  def isWall(position: Position) = !isFloor(position) && !isSink(position)
  def bounds: Position
}

case class ScalacobanGame(val user: Entity, val boxes: Set[Entity], val terrain: Level, val isFirst: Boolean) {
  def withUser(newUser: Entity) = ScalacobanGame(newUser, boxes, terrain, isFirst)

  def moveUser(m: Move): ScalacobanGame = {
    val newPosition = m.move(user.position)
    if (canMove(user, newPosition)) withUser(user.move(m))
    else {
      val optionGame = for { box <- getPushableBox(newPosition, m) } yield {
        val newBoxes = (boxes - box) + box.move(m)
        val newUser = user.move(m)
        ScalacobanGame(newUser, newBoxes, terrain, isFirst)
      }
      optionGame.getOrElse(this)
    }
  }

  def notFirst = ScalacobanGame(user, boxes, terrain, false)
  
  def isFinished: Boolean = {
    boxes forall { box => terrain.isSink(box.position) }
  }

  private def canMove(e: Entity, newPosition: Position) = {
    terrain.isFloor(newPosition) && !(boxes map { e: Entity => e.position } contains(newPosition))
  }
  
  private def getPushableBox(p: Position, m: Move): Option[Entity] = {
    boxes filter { b => b.position == p } filter { b => canMove(b, m.move(p)) } headOption
  }
}