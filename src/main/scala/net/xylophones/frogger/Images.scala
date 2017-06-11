package net.xylophones.frogger

import org.scalajs.dom.document

object Images {

  private val mediaPath = document.getElementById("frogger").getAttribute("mediaPath")
  
  private def fullPath(pth: String) = s"$mediaPath$pth"

  val tiles = new TiledImage(Image(fullPath("img/tiles.png")), 32, 32)
  val top = Image(fullPath("img/top.png"))
  val oneUp = Image(fullPath("img/1_up.png"))
  val highScore = Image(fullPath("img/high_score.png"))
  val nums = Image(fullPath("img/numbers.png"))
  val lifeImage = Image(fullPath("img/life.png"))
  val levelImage = Image(fullPath("img/level2.png"))
  val timeImage = Image(fullPath("img/time.png"))
  val frog = Image(fullPath("img/frog.png"))
  val deadFrog = Image(fullPath("img/deadfrog.png"))

}
