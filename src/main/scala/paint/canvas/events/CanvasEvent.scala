package paint.canvas.events

/**
  * Created by nic on 03/12/2016.
  */
sealed trait CanvasEvent

case class AddPoint(x: Double, y: Double) extends CanvasEvent
object Tick extends CanvasEvent
object Pause extends CanvasEvent
object Restart extends CanvasEvent