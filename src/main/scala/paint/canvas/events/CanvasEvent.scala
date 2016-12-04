package paint.canvas.events

import paint.Geometry.DoublePoint

/**
  * Created by nic on 03/12/2016.
  */
sealed trait CanvasEvent

case class AddPoint(position: DoublePoint, velocity: DoublePoint, size: Double) extends CanvasEvent
object Tick extends CanvasEvent
object Pause extends CanvasEvent
object Restart extends CanvasEvent
object Toggle extends CanvasEvent