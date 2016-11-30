package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import paint.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.canvas._
import paint.generator.{Generator, GeneratorState, StateEvent}
import paint.html.{NativeRenderingContext, RenderingContext, TransformedCanvasRenderingContext2D}

import scala.scalajs.js
import scala.util.Random
import scala.collection.immutable._

@JSExport
object Main {

    case class Size(w: Int, h: Int)

    @JSExport
    def main(htmlCanvas: dom.html.Canvas): Unit = {
        paint.Conf.canvasInitializer.initialise(htmlCanvas)
        val ctx = htmlCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]
        val canvas = paint.Conf.canvas(ctx)

        ctx.fillStyle = "white"

        val size = 0.1
        val noise = 4

        val gen = generator(2)
        var currentState  = initialState(htmlCanvas)
        var running = true
        var mouseDown = false

        htmlCanvas.onmousedown = (e: dom.MouseEvent) => {
            mouseDown = true
            val currentDrawing = if (running) currentState.drawing else CanvasDrawing.empty
            currentState = GeneratorState(
                new CanvasDrawing { override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = {
                    canvasAlgebra.sequence(
                        currentDrawing(canvasAlgebra),
                        canvasAlgebra.fillRect(e.clientX, e.clientY, size, size)
                    )
                }},
                currentState.frame
            )
            mouseDown = true
            running = true
        }

        htmlCanvas.onmousemove = (e: dom.MouseEvent) => if (mouseDown) {
            val currentDrawing = currentState.drawing
            currentState = GeneratorState(
                new CanvasDrawing { override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = {
                    canvasAlgebra.sequence(
                        currentDrawing(canvasAlgebra),
                        canvasAlgebra.fillRect(e.clientX, e.clientY, size, size)
                    )
                }},
                currentState.frame
            )
            running = true
        }

        htmlCanvas.onmouseup = (e: dom.MouseEvent) => {
            mouseDown = false
        }

        window.onkeydown = (e: dom.KeyboardEvent) => {
            if (e.keyCode == KeyCode.Space) {
                running = !running
            }
        }

        dom.window.setInterval(
            () => if (running) {
                for (i <- 1 to 100) {
                    currentState.drawing.apply(DrawHtmlAlgebra).apply(ctx)
                    currentState = gen.next(StateEvent(currentState, Unit))
                }
            },
            10
        )
    }

    def initialState(htmlCanvas: dom.html.Canvas, size: Int = 1): GeneratorState =
        GeneratorState(CanvasDrawing.empty, 0)

    def initialState2(htmlCanvas: dom.html.Canvas, size: Int = 1): GeneratorState = GeneratorState(
        new CanvasDrawing {
            override def apply[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
                canvasAlgebra.fillRect(htmlCanvas.width / 2 - 200, htmlCanvas.height / 2, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2, htmlCanvas.height / 2, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2 + 200, htmlCanvas.height / 2, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2, htmlCanvas.height / 2 + 200, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2, htmlCanvas.height / 2 - 200, size, size)
            )
        },
        0
    )

    def generator(noise: Double): Generator[Unit] = Generator(
        transition(noiseTransf(noise))
    )

    def transition(transf: PlaneTransformation)(se: StateEvent[Unit]): GeneratorState = {
        GeneratorState(
            se.state.drawing.apply(PlaneTransformationAlgebra(transf)),
            se.state.frame + 1
        )
    }

    def noiseTransf(noiseLevel: Double): PlaneTransformation = (point: DoublePoint) => {
        val shift = DoublePoint(
            Random.nextDouble() * 2 - 0.9,
            Random.nextDouble() * 2 - 1
        ) * noiseLevel
        point + shift
    }
    //            Random.nextDouble() * 2 * noiseLevel - noiseLevel / 1.05,
    //            Random.nextDouble() * 2 * noiseLevel / 8 - noiseLevel/8
}
