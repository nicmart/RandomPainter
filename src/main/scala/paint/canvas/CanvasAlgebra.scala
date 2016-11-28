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

trait CanvasDrawing[T] {
    def draw(canvasAlgebra: CanvasAlgebra[T]): T
}

object CanvasDrawing {
    def apply[T](drawing: CanvasAlgebra[T] => T): CanvasDrawing[T] = new CanvasDrawing[T] {
        override def draw(canvasAlgebra: CanvasAlgebra[T]): T = drawing(canvasAlgebra)
    }
}

object DrawHtmlCanvas extends CanvasAlgebra[CanvasRenderingContext2D => Unit] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double) =
        _.fillRect(x, y, w, h)
    override def sequence(ts: (CanvasRenderingContext2D => Unit)*) =
        context => {
            for (t <- ts) {
                t(context)
            }
        }
}

case class NoiseAdderAlgebra[T](radius: Double) extends CanvasAlgebra[CanvasDrawing[T]] {

    override def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing[T] = {
        val offsetX = Random.nextDouble() * 2 * radius - radius
        val offsetY = Random.nextDouble() * 2 * radius - radius

        new CanvasDrawing[T] {
            override def draw(canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.fillRect(
                x + offsetX,
                y + offsetY,
                w,
                h
            )
        }
    }

    override def sequence(ts: CanvasDrawing[T]*): CanvasDrawing[T] = new CanvasDrawing[T] {
        override def draw(canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            ts.map(drawing => drawing(canvasAlgebra)): _*
        )
    }
}

case class PlaneTransformationAlgebra[T](transformation: PlaneTransformation) extends CanvasAlgebra[CanvasDrawing[T]] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing[T] = {
        val newPoint = transformation(DoublePoint(x, y))
        new CanvasDrawing[T] {
            override def draw(canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.fillRect(
                newPoint.x,
                newPoint.y,
                w,
                h
            )
        }
    }
    override def sequence(ts: CanvasDrawing[T]*): CanvasDrawing[T] =  new CanvasDrawing[T] {
        override def draw(canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            ts.map(drawing => drawing(canvasAlgebra)): _*
        )
    }
}