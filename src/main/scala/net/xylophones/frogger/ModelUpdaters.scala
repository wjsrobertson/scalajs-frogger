package net.xylophones.frogger

object Config {
  val gameWidth = 512
  val gameHeight = 1200
  val frogWidth = 22
  val frogHeight = 22
  val channelHeight = 32
  val channelsHeight = channelHeight * 12
  val scorePanelHeight = 32 * 3
  val homeHeight = 16 * 3
  val bottomPanelHeight = 16 + 32
  val frogMinY = scorePanelHeight
  val frogMaxY = scorePanelHeight + homeHeight + channelsHeight - (channelHeight - frogHeight) / 2
}

trait ModelUpdater {
  def update(model: Model): Model
}

object ModelUpdaters {
  val updaters = Seq(ChannelUpdater, FrogUpdater)
}

object FrogUpdater extends ModelUpdater {
  def update(model: Model): Model = {
    val userDirection = UserInput.direction()

    val jumpDirection: Option[Direction.Dir] =
      if (model.frogJumpTimer > 0) Some(model.frogFacing)
      else if (userDirection.isDefined) userDirection
      else None

    val (fp, ff, ft) = jumpDirection match {
      case Some(d) =>
        val scale = 8
        val newFrogPosition = model.frogPosition.add(d.vector.scale(scale))
        val newFrogDirection = d
        val newFrogTimer = if (model.frogJumpTimer >= 1) model.frogJumpTimer - 1 else 3

        (newFrogPosition, newFrogDirection, newFrogTimer)
      case _ => (model.frogPosition, model.frogFacing, model.frogJumpTimer)
    }

    import Config._

    val yConstrained = fp match {
      case v: Vector if v.x < 0 => Vector(0, v.y)
      case v: Vector if v.x + frogWidth > gameWidth => Vector(gameWidth - frogWidth, fp.y)
      case _ => fp
    }

    val xyConstrained = yConstrained match {
      case v: Vector if (v.y + frogHeight) > frogMaxY => Vector(fp.x, frogMaxY - frogHeight)
      case v: Vector if v.y < frogMinY => Vector(fp.x, frogMinY)
      case _ => fp
    }

    model.copy(frogPosition = xyConstrained, frogFacing = ff, frogJumpTimer = ft)
  }
}

object ChannelUpdater extends ModelUpdater {
  def update(model: Model): Model = {
    val channelPositions: Seq[Vector] = (model.layers.all zip model.positions)
      .filter(_._1.isInstanceOf[Channel])
      .map(_._2)

    val cp = updatePositions(model.layers.channels, channelPositions)
    val ap = Layers.allPositions(cp)

    model.copy(positions = ap)
  }

  private def updatePositions(channels: Seq[Channel], positions: Seq[Vector]): Seq[Vector] = {
    channels zip positions map { case (c, p) =>
      val x = shunt(c, p.x + c.velocity)
      Vector(x, p.y)
    }
  }

  private def shunt(channel: Channel, x: Int): Int = {
    import channel._

    val mid = x + width/2

    if (velocity > 0 && x > 0) x - width/2
    else if (velocity < 0 && mid < 0) x + width/2
    else x
  }
}


