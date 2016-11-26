package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import paint.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.html.{NativeRenderingContext, RenderingContext, TransformedCanvasRenderingContext2D}

import scala.scalajs.js
import scala.util.Random
import scala.collection.immutable._

@JSExport
object Main {

    case class Size(w: Int, h: Int)

    @JSExport
    def main(htmlCanvas: dom.html.Canvas): Unit = {
        val ctx = htmlCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]
        paint.Conf.canvasInitializer.initialise(htmlCanvas)
        val canvas = paint.Conf.canvas(ctx)

        ctx.fillStyle = "white"
        val action1 = Canvas.drawSequence[RenderingContext](
            Canvas.drawPoint(Point(htmlCanvas.width / 2, htmlCanvas.height / 2), 10)
        )

        val transformation: PlaneTransformation = (point: DoublePoint) => point / 2
        val noiseLevel = 10;

        def noiseTransf(noiseLevel: Int): PlaneTransformation = (point: DoublePoint) => {
            val radius = noiseLevel / 2
            val shift = DoublePoint(
                Random.nextInt(noiseLevel) - radius,
                Random.nextInt(noiseLevel) - radius
            )
            point + shift
        }

        //val noiser = Canvas.drawAndTransform(noiseTransf(100)) _
        val noiser = Canvas.drawTransformed(noiseTransf(100)) _

        var action = Canvas.drawNothing
        var last = action1

        for (i <- 1 to 30) {
            last = noiser(last)
            action = Canvas.drawSequence(
                action,
                last
            )
        }

        canvas(action)
    }
}
