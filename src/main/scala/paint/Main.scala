package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import paint.Geometry.Point

import scala.scalajs.js
import scala.util.Random

@JSExport
object Main {

    case class Size(w: Int, h: Int)

    @JSExport
    def main(htmlCanvas: html.Canvas): Unit = {
        val ctx = htmlCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]
        paint.Conf.canvasInitializer.initialise(htmlCanvas)
        val canvas = paint.Conf.canvas(ctx)

        ctx.fillStyle = "white"
        val action = Canvas.drawSequence[dom.CanvasRenderingContext2D](
            Canvas.drawPoint(Point(100, 100), 10),
            Canvas.drawPoint(Point(200, 100), 10)
        )

        canvas(action)
    }
}
