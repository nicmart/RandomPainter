package paint.canvas

import org.scalajs.dom.CanvasRenderingContext2D
import paint.Geometry.{DoublePoint, PlaneTransformation}
import paint.algebra.Expression

import scala.util.Random

/**
  * Created by nic on 28/11/2016.
  */
trait CanvasAlgebra[T] {
    def fillRect(x: Double, y: Double, w: Double, h: Double): T
    def sequence(ts: T*): T
}

trait CanvasDrawing extends Expression[CanvasAlgebra] {
    def apply[T](canvasAlgebra: CanvasAlgebra[T]): T
}

sealed trait CanvasExpr
case class FillRect(x: Double, y: Double, w: Double, h: Double) extends CanvasExpr
case class Sequence(ts: CanvasExpr *) extends CanvasExpr

object ExprCanvasAlgebra extends CanvasAlgebra[CanvasExpr] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasExpr = FillRect(x, y, w, h)
    override def sequence(ts: CanvasExpr*): CanvasExpr = Sequence(ts: _*)

    def toDrawing(expr: CanvasExpr): CanvasDrawing = expr match {
        case FillRect(x, y, w, h) => CanvasDrawing.fillRect(x, y, w, h)
        case s: Sequence => {
            val drawings = s.ts.map(toDrawing)
            new CanvasDrawing {
                override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
                    drawings.map((drawing: CanvasDrawing) => drawing.apply(canvasAlgebra)): _*
                )
            }
        }
    }
}

object CanvasDrawing {
    val empty = new CanvasDrawing {
        override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence()
    }

    def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing = new CanvasDrawing {
        override def apply[T](canvasAlgebra: CanvasAlgebra[T]) =
            canvasAlgebra.fillRect(x, y, w, h)
    }

    def sequence(drawing1: CanvasDrawing, drawing2: CanvasDrawing): CanvasDrawing = new CanvasDrawing {
        override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            drawing1.apply(canvasAlgebra),
            drawing2.apply(canvasAlgebra)
        )
    }

    def flatten(drawing: CanvasDrawing): CanvasDrawing = {
        ExprCanvasAlgebra.toDrawing(drawing.apply(ExprCanvasAlgebra))
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

case class  PlaneTransformationAlgebra(transformation: PlaneTransformation) extends CanvasAlgebra[CanvasDrawing] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): CanvasDrawing = {
        val newPoint = transformation(DoublePoint(x, y))
        new CanvasDrawing {
            override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.fillRect(
                newPoint.x,
                newPoint.y,
                w,
                h
            )
        }
    }
    override def sequence(ts: CanvasDrawing*): CanvasDrawing =  new CanvasDrawing {
        override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
            ts.map(drawing => drawing.apply(canvasAlgebra)): _*
        )
    }
}

object NumberOfOperations extends CanvasAlgebra[Int] {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): Int = 1
    override def sequence(ts: Int*): Int = ts.sum
}