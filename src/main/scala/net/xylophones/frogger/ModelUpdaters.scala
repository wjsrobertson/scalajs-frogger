package net.xylophones.frogger

abstract class ModelUpdater(states: Seq[PlayState.State]) {
  def updateIfApplicable(model: Model): Model =
    if (states.contains(model.playState)) update(model)
    else model

  protected def update(model: Model): Model
}

object ModelUpdaters {
  val updaters = Seq(TimerUpdater, ChannelUpdater, FrogMoveUpdater, FrogChannelLander, FrogPositionConstrainer,
    FrogCollisionChecker, NextLifeUpdater, HighScoreModelUpdater, FrogHomeLander, NextLevelUpdater, NewGameUpdater)
}

object FrogMoveUpdater extends ModelUpdater(PlayState.inGameStates) {
  def update(model: Model): Model = {
    val userDirection = UserInput.direction()

    val jumpDirection: Option[Direction.Dir] =
      if (model.frogJumpTimer > 0) Some(model.frogFacing)
      else if (userDirection.isDefined && model.playState == PlayState.InPlay) userDirection
      else None

    jumpDirection match {
      case Some(d) =>
        val scale = 8
        val newFrogPosition = model.frogPosition.add(d.vector.scale(scale))
        val newFrogDirection = d
        val newFrogTimer = if (model.frogJumpTimer >= 1) model.frogJumpTimer - 1 else 3
        val newScore = if (newFrogTimer == 0) model.score + Config.pointsForJump else model.score
        val jumpSound = if (newFrogTimer == 0) Seq(Sounds.Jump) else Seq.empty

        model.copy(
          frogPosition = newFrogPosition,
          frogFacing = newFrogDirection,
          frogJumpTimer = newFrogTimer,
          score = newScore,
          sounds = model.sounds ++ jumpSound
        )
      case _ => model
    }
  }
}

object FrogPositionConstrainer extends ModelUpdater(Seq(PlayState.InPlay)) {
  def update(model: Model): Model = {
    import Config._

    val yConstrained = model.frogPosition match {
      case v: Vector if v.x < 0 => Vector(0, v.y)
      case v: Vector if v.x + frogWidth > gameWidth => Vector(gameWidth - frogWidth, v.y)
      case _ => model.frogPosition
    }

    val xyConstrained = yConstrained match {
      case v: Vector if (v.y + frogHeight) > frogMaxY => Vector(v.x, frogMaxY - frogHeight)
      case v: Vector if v.y < frogMinY => Vector(v.x, frogMinY)
      case _ => yConstrained
    }

    model.copy(frogPosition = xyConstrained)
  }
}

object FrogChannelLander extends ModelUpdater(PlayState.inGameStates) {
  def update(model: Model): Model = {

    val landingChannel = model.channelsWithPositions()
      .find(x => ChannelCollisionChecker.isLanding(x._1, x._2, model.layers.frog, model.frogPosition))
      .map(_._1)

    landingChannel.map(ch => ch.velocity)
      .map(vel => model.frogPosition.add(vel, 0))
      .map(fp => model.copy(frogPosition = fp))
      .getOrElse(model)
  }
}

object FrogCollisionChecker extends ModelUpdater(Seq(PlayState.InPlay)) {
  def update(model: Model): Model = {
    import ChannelCollisionChecker._

    val deadlyCollision =
      model.channelsWithPositions().exists(cp => isDeadlyCollision(cp._1, cp._2, model.layers.frog, model.frogPosition)) ||
        model.homesWithPositions().exists(hp => isDeadlyCollision(hp._1, hp._2, model.layers.frog, model.frogPosition))

    if (deadlyCollision)
      model.copy(frogDeathTimer = Config.frogDeathTime, playState = PlayState.FrogDeathAnimation, sounds = model.sounds :+ Sounds.DieInWater)
    else model
  }
}

object FrogHomeLander extends ModelUpdater(Seq(PlayState.InPlay)) {

  import ChannelCollisionChecker._

  def update(model: Model): Model = model
    .homesWithPositions()
    .zipWithIndex
    .find(hpi => isLanding(hpi._1._1, hpi._1._2, model.layers.frog, model.frogPosition))
    .map { case ((h, p), i) =>
      val newHome = HomeFactory.create(i, HomeContent.Frog)
      val newHomes = model.layers.homes.patch(i, Seq(newHome), 1)
      // TODO - add on score for unused seconds
      // TODO - a lot of this "reset" values are common to other ModelUpdaters - maybe factor out?
      // TODO - no magic values
      model.copy(
        layers = model.layers.copy(homes = newHomes),
        score = model.score + 50,
        frogPosition = Layers.initialFrogPosition,
        levelStartTimeMs = System.currentTimeMillis(),
        frogJumpTimer = 0,
        frogFacing = Direction.Up,
        sounds = model.sounds :+ Sounds.Home
      )
    }.getOrElse(model)
}

object NextLifeUpdater extends ModelUpdater(PlayState.inGameStates) {
  def update(model: Model): Model = {
    if (model.frogDeathTimer == 1) {
      if (model.lives == 1) {
        model.copy(playState = PlayState.NotInPlay, frogDeathTimer = 0, lives = 0)
      } else {
        model.copy(lives = model.lives - 1, frogPosition = Layers.initialFrogPosition, frogDeathTimer = 0,
          playState = PlayState.InPlay, frogJumpTimer = 0, frogFacing = Direction.Up,
          levelStartTimeMs = System.currentTimeMillis())
      }
    } else model
  }
}

object TimerUpdater extends ModelUpdater(PlayState.inGameStates) {
  def update(model: Model): Model = {
    if (model.frogDeathTimer > 1)
      model.copy(frogDeathTimer = model.frogDeathTimer - 1)
    else if (model.levelDurationMs() > Config.levelTimeLimitMs && model.frogDeathTimer == 0)
           model.copy(frogDeathTimer = Config.frogDeathTime, playState = PlayState.FrogDeathAnimation) // make it so you don't have to set these at same time
    else model
  }
}

object ChannelUpdater extends ModelUpdater(PlayState.all) {
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

    val mid = x + width / 2

    if (velocity > 0 && x > 0) x - width / 2
    else if (velocity < 0 && mid < 0) x + width / 2
    else x
  }
}

object HighScoreModelUpdater extends ModelUpdater(PlayState.inGameStates) {
  def update(model: Model): Model = {
    if (model.score > model.highScore) model.copy(highScore = model.score)
    else model
  }
}

object NextLevelUpdater extends ModelUpdater(PlayState.inGameStates) {
  def update(model: Model): Model = {
    if (model.layers.homes.count(_.content == HomeContent.Frog) == 5)
      model.copy(
        level = model.level + 1,
        score = model.score + 1000,
        layers = model.layers.copy(
          homes = (0 to 5).map(HomeFactory.create),
          channels = Channel.channels(model.level + 1)
        ),
        frogPosition = Layers.initialFrogPosition,
        levelStartTimeMs = System.currentTimeMillis(),
        frogJumpTimer = 0,
        frogFacing = Direction.Up
      )
    else model
  }
}

// TODO - common with code above execpt lives
object NewGameUpdater extends ModelUpdater(PlayState.all) {
  def update(model: Model): Model = {
    if (UserInput.newGame()) {
      model.copy(
        level = 1,
        score = 0,
        layers = model.layers.copy(
          homes = (0 to 5).map(HomeFactory.create),
          channels = Channel.channels(1)
        ),
        frogPosition = Layers.initialFrogPosition,
        levelStartTimeMs = System.currentTimeMillis(),
        frogJumpTimer = 0,
        frogFacing = Direction.Up,
        lives = 3,
        frogDeathTimer = 0,
        playState = PlayState.InPlay,
        sounds = model.sounds :+ Sounds.LadyFrog
      )
    }
    else model
  }
}

/*
Every safe step scores 10 points, and every frog arriving home scores 50 points plus 10 per unused second.
Guiding a lady frog home or eating a fly scores 200 points), and when all
5 frogs are home to end the level the player earns 1,000 points.
Players earn an extra life at 10,000 or 20,000 points, and none thereafter.
 */
