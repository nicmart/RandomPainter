package paint

import org.scalajs.dom.html
import org.scalajs.dom.CanvasRenderingContext2D
import paint.Canvas.CanvasAction
import paint.Geometry._

/**
  * Created by nic on 26/11/2016.
  */
trait Canvas[C] { self =>
    type Action = CanvasAction[C]
    def apply(action: Action): Canvas[C]
}

object Canvas {
    type CanvasAction[C] = (C => Unit)
    type PrimitiveCanvasAction = CanvasAction[CanvasRenderingContext2D]

    def drawPoint(p: Point, size: Int): PrimitiveCanvasAction = {
        val radius = size / 2
        val vector = Point(radius, radius)
        drawSquareCentredAt(p, radius)
    }

    def drawRect(p1: Point, p2: Point): PrimitiveCanvasAction = (c: CanvasRenderingContext2D) => {
        c.fillRect(p1.x.toDouble, p1.y.toDouble, p2.x.toDouble, p2.y.toDouble)
    }

    def drawSquareCentredAt(p: Point, radius: Int): PrimitiveCanvasAction = (c: CanvasRenderingContext2D) => {
        val vector = Point(radius, radius) / 2
        val topLeft = p - vector
        c.fillRect(topLeft.x.toDouble, topLeft.y.toDouble, radius.toDouble, radius.toDouble)
    }

    def drawSequence[C](actions: CanvasAction[C]*): CanvasAction[C] = (c: C) => {
        for (action <- actions) {
            action(c)
        }
    }
}

case class PrimitiveCanvas(canvasContext: CanvasRenderingContext2D) extends Canvas[CanvasRenderingContext2D]
{
    override def apply(action: CanvasAction[CanvasRenderingContext2D]): PrimitiveCanvas = {
        action.apply(canvasContext)
        this
    }
}
