package com.jk.game

import scala.annotation.tailrec

trait Game[Input, State] {
  def readInput: Input
  def update(input: => Input, state: State): State
  def draw(state: State)
  def shouldQuit(state: State): Boolean

  @tailrec
  final def run(state: State): State = {
    val nextState = update(readInput, state)
    draw(nextState)
    if (!shouldQuit(nextState)) {
      run(nextState)
    } else {
      nextState
    }
  }
}