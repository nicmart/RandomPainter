package paint.canvas

import org.scalajs.dom.CanvasRenderingContext2D
import paint.coroutine.CoRoutine
import paint.geometry.Geometry.DoublePoint

/**
  * Created by nic on 06/12/2016.
  */
object CanvasCoRoutine {

    def drawPoint[A](size: Double, co: CoRoutine[A, DoublePoint]): CoRoutine[A, CanvasRenderingContext2D => Unit] =
        co.map { point =>
            { context =>
                context.fillRect(point.x, point.y, size, size)
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
