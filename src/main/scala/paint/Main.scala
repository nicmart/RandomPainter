package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import paint.geometry.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.algebra.{DrawOnCanvasPaintAlgebra, LineDrawOnCanvasPaintAlgebra, NumberOfOperations}
import paint.events._
import paint.generative._
import paint.html.{NativeRenderingContext, RenderingContext, TransformedCanvasRenderingContext2D}
import paint.portfolio.Portfolio

import scala.scalajs.js
import scala.util.Random
import scala.collection.immutable._

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

        var drawing: Drawing[CanvasEvent] = Portfolio(canvasSize).gravity
        var mouseDown = false

        val iterations = 10

        def draw(t: Double): Unit = {
            for (i <- 1 to iterations) {
                drawing.draw().apply(ctx)
                drawing = drawing.event(Tick)
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
            drawing = drawing.event(drawing.newPoint(DoublePoint(e.clientX, e.clientY)))
        }

        htmlCanvas.onmousemove = (e: dom.MouseEvent) => if (mouseDown) {
            drawing = drawing.event(drawing.newPoint(DoublePoint(e.clientX, e.clientY)))
        }

        htmlCanvas.onmouseup = (e: dom.MouseEvent) => {
            mouseDown = false
        }

        window.onkeydown = (e: dom.KeyboardEvent) => {
            if (e.keyCode == KeyCode.Space) {
                drawing = drawing.event(Toggle)
            }
        }
        dom.window.requestAnimationFrame(draw)
    }
}
