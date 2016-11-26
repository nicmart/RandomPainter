package paint

import org.scalajs.dom.html
import org.scalajs.dom.CanvasRenderingContext2D
import paint.Canvas.CanvasAction
import paint.Geometry._
import paint.html._

/**
  * Created by nic on 26/11/2016.
  */
trait Canvas[C] { self =>
    type Action = CanvasAction[C]
    def apply(action: Action): Canvas[C]
}

object Canvas {
    type CanvasAction[C] = (C => Unit)
    type PrimitiveCanvasAction = CanvasAction[RenderingContext]

    def drawPoint(p: Point, size: Int): PrimitiveCanvasAction = {
        val radius = size / 2
        val vector = Point(radius, radius)
        drawSquareCentredAt(p, radius)
    }

    def drawNothing: PrimitiveCanvasAction = (c: RenderingContext) => Unit

    def drawRect(p1: Point, p2: Point): PrimitiveCanvasAction = (c: RenderingContext) => {
        c.fillRect(p1.x.toDouble, p1.y.toDouble, p2.x.toDouble, p2.y.toDouble)
    }

    def drawSquareCentredAt(p: Point, radius: Int): PrimitiveCanvasAction = (c: RenderingContext) => {
        val vector = Point(radius, radius) / 2
        val topLeft = p - vector
        c.fillRect(topLeft.x.toDouble, topLeft.y.toDouble, radius.toDouble, radius.toDouble)
    }

    def drawSequence[C](actions: CanvasAction[C]*): CanvasAction[C] = (c: C) => {
        for (action <- actions) {
            action(c)
        }
    }

    def drawTransformed(t: PlaneTransformation)(action: PrimitiveCanvasAction): CanvasAction[RenderingContext] = (c: RenderingContext) => {
        action(new TransformedCanvasRenderingContext2D(c, t))
    }

    def drawAndTransform(t: PlaneTransformation)(action: PrimitiveCanvasAction)
    : CanvasAction[RenderingContext] =
        (c: RenderingContext) => {
            action(c)
            action(new TransformedCanvasRenderingContext2D(c, t))
        }
}

case class PrimitiveCanvas(canvasContext: RenderingContext) extends Canvas[RenderingContext]
{
    override def apply(action: CanvasAction[RenderingContext]): PrimitiveCanvas = {
        action.apply(canvasContext)
        this
    }
}
