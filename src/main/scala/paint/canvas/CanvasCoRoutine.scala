package paint.canvas

import org.scalajs.dom.CanvasRenderingContext2D
import paint.coroutine.CoRoutine
import paint.geometry.Geometry.DoublePoint

/**
  * Created by nic on 06/12/2016.
  */
object CanvasCoRoutine {

    def empty[A]: CoRoutine[A, CanvasRenderingContext2D => Unit] = CoRoutine.const(_ => ())

    def drawPoint[A](size: Double, co: CoRoutine[A, DoublePoint]): CoRoutine[A, CanvasRenderingContext2D => Unit] =
        co.map { point =>
            { context =>
                //context.fillRect(point.x, point.y, size, size)
                context.lineWidth = size
                context.strokeStyle = "white"
                context.beginPath()
                context.lineTo(point.x, point.y)
                context.stroke()
            }
        }

    def drawPath[A](size: Double, co: CoRoutine[A, Vector[DoublePoint]]): CoRoutine[A, CanvasRenderingContext2D => Unit] =
        co.map { points =>
            { context =>
                if (points.length > 1) {
                    context.lineWidth = size
                    context.strokeStyle = "white"
                    context.beginPath()
                    context.moveTo(points.head.x, points.head.y)
                    for (point <- points.drop(1)) {
                        context.lineTo(point.x, point.y)
                    }
                    context.stroke()
                } else {
                    ()
                }
            }
        }

    def drawPathWithSize[A](pointsWithSize: CoRoutine[A, (Double, Vector[DoublePoint])]): CoRoutine[A, CanvasRenderingContext2D => Unit] =
        pointsWithSize.map { case (size, points) =>
        { context =>
            if (points.length > 1) {
                context.lineWidth = size
                context.strokeStyle = "white"
                context.beginPath()
                context.moveTo(points.head.x, points.head.y)
                for (point <- points.drop(1)) {
                    context.lineTo(point.x, point.y)
                }
                context.stroke()
            } else {
                ()
            }
        }
        }

    def append[A](
        drawing: CoRoutine[A, CanvasRenderingContext2D => Unit],
        to: CoRoutine[A, CanvasRenderingContext2D => Unit]
    ): CoRoutine[A, CanvasRenderingContext2D => Unit] =
        to.zipWith(drawing).map { case (d1, d2) =>
            canvas => {
                d1(canvas)
                d2(canvas)
            }
        }
}
