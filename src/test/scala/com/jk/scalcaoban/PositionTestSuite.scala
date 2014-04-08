package com.jk.scalcaoban

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.jk.scalacoban._

@RunWith(classOf[JUnitRunner])
class PositionTestSuite extends FunSuite {
  test("Moving position should move position") {
    val p = Position(3, 3)

    assert(MoveLeft.move(p) === Position(2, 3))
    assert(MoveRight.move(p) === Position(4, 3))
    assert(MoveUp.move(p) === Position(3, 2))
    assert(MoveDown.move(p) === Position(3, 4))
  }
  
  test("Moving entity should move entity") {
    val e = new Entity(Position(3, 3))
    
    e.move(MoveLeft)
    assert(e.position === Position(2, 3))
    e.move(MoveRight)
    assert(e.position === Position(3, 3))
    e.move(MoveUp)
    assert(e.position === Position(3, 2))
    e.move(MoveDown)
    assert(e.position === Position(3, 3))
  }
}
