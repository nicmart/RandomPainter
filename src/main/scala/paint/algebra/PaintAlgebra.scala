package paint.algebra

import org.scalajs.dom.CanvasRenderingContext2D
import paint.geometry.Geometry.DoublePoint

/**
  * Created by nic on 04/12/2016.
  */
trait PaintAlgebra[T] {
    def point(position: DoublePoint, velocity: DoublePoint, size: Double): T
    def sequence(expressions: List[T]): T
}

trait PaintDrawing extends Expression[PaintAlgebra] {
    def apply[T](paintAlgebra: PaintAlgebra[T]): T
    def flatMap(f: Point => PaintDrawing): PaintDrawing = apply(PointTransformationAlgebra(f))
}

sealed trait PaintAlgebraExpr
case class Point(position: DoublePoint, velocity: DoublePoint, size: Double) extends PaintAlgebraExpr
case class Sequence(expressions: List[PaintAlgebraExpr]) extends PaintAlgebraExpr

object PaintDrawing {

    val empty: PaintDrawing = new PaintDrawing {
        override def apply[T](paintAlgebra: PaintAlgebra[T]) = paintAlgebra.sequence(List())
    }

    def point(position: DoublePoint, velocity: DoublePoint, size: Double): PaintDrawing = new PaintDrawing {
        override def apply[T](paintAlgebra: PaintAlgebra[T]) = paintAlgebra.point(
            position, velocity, size
        )
    }

    def point(p: Point): PaintDrawing = point(p.position, p.velocity, p.size)

    def sequence(drawings: List[PaintDrawing]): PaintDrawing = new PaintDrawing {
        override def apply[T](paintAlgebra: PaintAlgebra[T]) = paintAlgebra.sequence(
            drawings.map(_.apply(paintAlgebra))
        )
    }
}

object DrawOnCanvasPaintAlgebra extends PaintAlgebra[CanvasRenderingContext2D => Unit] {

    override def point(position: DoublePoint, velocity: DoublePoint, size: Double): (CanvasRenderingContext2D) => Unit = {
        context => context.fillRect(position.x, position.y, size, size)
    }
    override def sequence(expressions: List[(CanvasRenderingContext2D) => Unit]): (CanvasRenderingContext2D) => Unit = {
        context => for (expression <- expressions) {
            expression(context)
        }
    }
}

object LineDrawOnCanvasPaintAlgebra extends PaintAlgebra[CanvasRenderingContext2D => Unit] {

    override def point(position: DoublePoint, velocity: DoublePoint, size: Double): (CanvasRenderingContext2D) => Unit = {
        context => {
            val from = position - velocity
            context.beginPath()
            context.lineWidth = size
            context.moveTo(from.x, from.y)
            context.lineTo(position.x, position.y)
            context.stroke()
        }
    }
    override def sequence(expressions: List[(CanvasRenderingContext2D) => Unit]): (CanvasRenderingContext2D) => Unit = {
        context => for (expression <- expressions) {
            expression(context)
        }
    }
}

object NumberOfOperations extends PaintAlgebra[Int] {
    override def point(position: DoublePoint, velocity: DoublePoint, size: Double): Int = 1
    override def sequence(expressions: List[Int]): Int = expressions.sum
}

/**
  * Transform the single point drawings in other drawings
  */
case class PointTransformationAlgebra(transf: Point => PaintDrawing) extends PaintAlgebra[PaintDrawing] {
    override def point(position: DoublePoint, velocity: DoublePoint, size: Double): PaintDrawing = {
        transf(Point(position, velocity, size))
    }
    override def sequence(expressions: List[PaintDrawing]): PaintDrawing = PaintDrawing.sequence(expressions)
}

