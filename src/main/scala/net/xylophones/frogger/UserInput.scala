package net.xylophones.frogger

import net.xylophones.frogger.FrogFacing.Direction

import scala.scalajs.js
import js.Dynamic.global
import org.scalajs.dom

import scala.collection.mutable

object Direction {
  sealed abstract class Dir(val keyCode: Int, val vector: Vector, val dir: FrogFacing.Direction)
  case object Up extends Dir(38, Vector(0, 1), FrogFacing.Up)
  case object Down extends Dir(40, Vector(0, -1), FrogFacing.Down)
  case object Left extends Dir(37, Vector(-1, 0), FrogFacing.Left)
  case object Right extends Dir(39, Vector(1, 0), FrogFacing.Right)

  def directions: Seq[Dir] = Seq(Up,Down,Left,Right)

  def fromKeyCode(keyCode: Int) = directions.find(_.keyCode == keyCode)
}

object UserInput {

  private val keys = mutable.Set[Int]()

  global.addEventListener("keydown", (e: dom.KeyboardEvent) => {
    keys += e.keyCode
  }, false)

  global.addEventListener("keyup", (e: dom.KeyboardEvent) => {
    keys -= e.keyCode
  }, false)

  def direction() = keys.flatMap(Direction.fromKeyCode(_)).headOption
}
