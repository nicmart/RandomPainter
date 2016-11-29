package paint.canvas

import org.scalajs.dom.CanvasRenderingContext2D
import paint.Geometry.{DoublePoint, PlaneTransformation}

import scala.util.Random

/**
  * Created by nic on 28/11/2016.
  */
trait CanvasAlgebra[T] {
    def fillRect(x: Double, y: Double, w: Double, h: Double): T
    def sequence(ts: T*): T
}

trait CanvasDrawing {
    def draw[T](canvasAlgebra: CanvasAlgebra[T]): T
}

object CanvasDrawing {
    val empty = new CanvasDrawing {
        override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence()
    }

    def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing = new CanvasDrawing {
        override def draw[T](canvasAlgebra: CanvasAlgebra[T]) =
            canvasAlgebra.fillRect(x, y, w, h)
    }

    def append(drawing1: CanvasDrawing, drawing2: CanvasDrawing): CanvasDrawing = new CanvasDrawing {
        override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            drawing1.draw(canvasAlgebra),
            drawing2.draw(canvasAlgebra)
        )
    }
}

object DrawHtmlAlgebra extends CanvasAlgebra[CanvasRenderingContext2D => Unit] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double) =
        _.fillRect(x, y, w, h)
    override def sequence(ts: (CanvasRenderingContext2D => Unit)*) =
        context => {
            for (t <- ts) {
                t(context)
            }
        }
}

case class NoiseAdderAlgebra(radius: Double) extends CanvasAlgebra[CanvasDrawing] {

    override def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing = {
        val offsetX = Random.nextDouble() * 2 * radius - radius
        val offsetY = Random.nextDouble() * 2 * radius - radius

        new CanvasDrawing {
            override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.fillRect(
                x + offsetX,
                y + offsetY,
                w,
                h
            )
        }
    }

    override def sequence(ts: CanvasDrawing*): CanvasDrawing = new CanvasDrawing {
        override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            ts.map(drawing => drawing.draw(canvasAlgebra)): _*
        )
    }
}

case class PlaneTransformationAlgebra(transformation: PlaneTransformation) extends CanvasAlgebra[CanvasDrawing] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing = {
        val newPoint = transformation(DoublePoint(x, y))
        new CanvasDrawing {
            override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.fillRect(
                newPoint.x,
                newPoint.y,
                w,
                h
            )
        }
    }
    override def sequence(ts: CanvasDrawing*): CanvasDrawing =  new CanvasDrawing {
        override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            ts.map(drawing => drawing.draw(canvasAlgebra)): _*
        )
    }
}