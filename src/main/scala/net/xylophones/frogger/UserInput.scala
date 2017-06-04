package net.xylophones.frogger

import scala.scalajs.js
import js.Dynamic.global
import org.scalajs.dom

import scala.collection.mutable

object UserInput {

  private val directionKeyCodes = Map(
    38 -> Direction.Up,
    40 -> Direction.Down,
    37 -> Direction.Left,
    39 -> Direction.Right)

  private val keys = mutable.Set[Int]()

  global.addEventListener("keydown", (e: dom.KeyboardEvent) => {
    keys += e.keyCode
  }, false)

  global.addEventListener("keyup", (e: dom.KeyboardEvent) => {
    keys -= e.keyCode
  }, false)

  def direction(): Option[Direction.Dir] = keys.flatMap(directionKeyCodes.get).headOption

  def newGame(): Boolean = keys.contains(78)

  def fullScreen(): Boolean = keys.contains(70)
}
