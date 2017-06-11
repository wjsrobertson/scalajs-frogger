package net.xylophones.frogger

import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLAudioElement

class Audio(element: HTMLAudioElement) {
  def play() = {
    stop()
    element.play()
  }

  def stop() = {
    element.currentTime = 0
    element.pause()
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

  def stopMusic(): Unit = for {
    (s, a) <- sounds if s.soundType == SoundType.Music
  } a.stop()

  def play(sound: Sounds.Sound): Unit = {
    if (sound.soundType == SoundType.Music)
      stopMusic()

    sounds(sound).play()
  }
}

object SoundType {

  sealed abstract class Type

  case object Fx extends SoundType.Type

  case object Music extends SoundType.Type
}

object Sounds {

  sealed abstract class Sound(val path: String, val soundType: SoundType.Type)

  case object LadyFrog extends Sound("sounds/lady.wav", SoundType.Fx)

  case object Home extends Sound("sounds/home.wav", SoundType.Fx)

  case object Jump extends Sound("sounds/jump.wav", SoundType.Fx)

  case object DieInWater extends Sound("sounds/die_water.wav", SoundType.Fx)

  case object DieOnRoad extends Sound("sounds/die_road.wav", SoundType.Fx)

  case object GameOver extends Sound("sounds/Music/GameOver.ogg", SoundType.Music)

  case object Start extends Sound("sounds/Music/Start.ogg", SoundType.Music)

  case object Theme extends Sound("sounds/Music/Theme.ogg", SoundType.Music)

  case object Home0 extends Sound("sounds/Music/Home0.ogg", SoundType.Music)

  case object Home1 extends Sound("sounds/Music/Home1.ogg", SoundType.Music)

  case object Home2 extends Sound("sounds/Music/Home2.ogg", SoundType.Music)

  case object Home3 extends Sound("sounds/Music/Home3.ogg", SoundType.Music)

  case object Home4 extends Sound("sounds/Music/Home4.ogg", SoundType.Music)

  case object Home5 extends Sound("sounds/Music/Home5.ogg", SoundType.Music)

  private val soundsForHomes = Map(0 -> Home0, 1 -> Home1, 2 -> Home2, 3 -> Home3, 4 -> Home4, 5 -> Home5)

  val all: Seq[Sound] = Seq(LadyFrog, Home, Jump, DieInWater, DieOnRoad, GameOver, Start, Theme, Home0, Home1, Home2, Home3, Home4, Home5)

  val music: Seq[Sound] = all.filter(_.soundType == SoundType.Music)

  def soundForHome(id: Int): Sound = soundsForHomes.getOrElse(id, Home5)
}
