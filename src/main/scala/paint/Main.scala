package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import paint.canvas.CanvasCoRoutine
import paint.coroutine.CoRoutine
import paint.geometry.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.portfolio.CoRoutinePortfolio

@JSExport
object Main {
    @JSExport
    def main(htmlCanvas: dom.html.Canvas): Unit = {
        paint.Conf.canvasInitializer.initialise(htmlCanvas)
        val ctx = htmlCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]

        ctx.fillStyle = "white"
        ctx.strokeStyle = "white"
        ctx.lineCap = "square"
        ctx.lineJoin = "miter"
        ctx.miterLimit = 20

        val canvasSize = DoublePoint(htmlCanvas.width, htmlCanvas.height)

        var drawing: CoRoutine[Unit, CanvasRenderingContext2D => Unit] =
            CoRoutine.const(_ => ())

        val iterations = 50

        var mouseDown = false

        def draw(t: Double): Unit = {

            for (i <- 1 to iterations) {
                val (drw, nextDrawing) = drawing.run(())
                drawing = nextDrawing
            }

            window.requestAnimationFrame(draw)

            /*
           var previoust: Double = 0
            var lastPrint: Double = 0
            if (t - lastPrint > 500) {
                val numPoints = currentState.drawing.apply(NumberOfOperations)
                val pointsPerSecond = numPoints * iterations / (t - previoust)
                val iterationsPerSecond = iterations / (t - previoust)
                println(
                    s"""
                       |${pointsPerSecond.toInt} points per millisecond,
                       |${iterationsPerSecond.toInt} iterations per millisecond,
                       |${1000 / (t - previoust)} FPS,
                       |${numPoints.toString} points
                     """.stripMargin)
                lastPrint = t
            }
            previoust = t*/
        }

        htmlCanvas.onmousedown = (e: dom.MouseEvent) => {
            mouseDown = true
            drawing = CanvasCoRoutine.append(
                drawing,
                drawingFromMouseEvent(e)
            )
        }

        htmlCanvas.onmousemove = (e: dom.MouseEvent) => if (mouseDown) {
            drawing = CanvasCoRoutine.append(
                drawing,
                drawingFromMouseEvent(e)
            )
        }

        htmlCanvas.onmouseup = (e: dom.MouseEvent) => {
            mouseDown = false
        }



        window.onkeydown = (e: dom.KeyboardEvent) => {
            if (e.keyCode == KeyCode.Space) {
                drawing = CanvasCoRoutine.empty
            }
        }
        dom.window.requestAnimationFrame(draw)
    }

    def drawingFromMouseEvent(e: dom.MouseEvent) =
        CoRoutinePortfolio.test9(DoublePoint(e.clientX, e.clientY) * 2)
}
