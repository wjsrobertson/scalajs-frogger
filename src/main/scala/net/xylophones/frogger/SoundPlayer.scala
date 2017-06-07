package net.xylophones.frogger

import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLAudioElement

class Audio(element: HTMLAudioElement) {
  def play() = {
    element.currentTime = 0
    element.play()
  }
}

object Audio {
  def apply(src: String): Audio = {
    val element = document.createElement("audio").asInstanceOf[HTMLAudioElement]
    element.src = src
    new Audio(element)
  }
}

object SoundPlayer {
  private val sounds: Map[Sounds.Sound, Audio] = Sounds.all.map(s => s -> Audio(s.path)).toMap

  def play(sound: Sounds.Sound) = sounds(sound).play()
}

object Sounds {

  sealed abstract class Sound(val path: String)

  case object LadyFrog extends Sound("sounds/lady.wav")

  case object Home extends Sound("sounds/home.wav")

  case object Jump extends Sound("sounds/jump.wav")

  case object DieInWater extends Sound("sounds/die_water.wav")

  case object DieOnRoad extends Sound("sounds/die_road.wav")

  def all = Seq(LadyFrog, Home, Jump, DieInWater, DieOnRoad)
}
