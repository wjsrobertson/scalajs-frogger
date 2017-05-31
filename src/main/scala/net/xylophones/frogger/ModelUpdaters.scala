package net.xylophones.frogger

trait ModelUpdater {
  def update(model: Model, layers: Layers): Model
}

object FrogUpdater extends ModelUpdater {
  def update(model: Model, layers: Layers): Model = {
    ???
  }
}
