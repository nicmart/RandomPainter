package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import paint.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.canvas.{CanvasAlgebra, CanvasDrawing, DrawHtmlAlgebra, PlaneTransformationAlgebra}
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

        val gen = generator()
        var currentState  = initialState(htmlCanvas)
        var running = true

        htmlCanvas.onmouseup = (e: dom.MouseEvent) => {
            val currentDrawing = if (running) currentState.drawing else CanvasDrawing.empty
            currentState = GeneratorState(
                CanvasDrawing.append(
                    currentDrawing,
                    CanvasDrawing.fillRect(e.clientX, e.clientY, 0.4, 0.4)
                ),
                currentState.frame
            )
            running = true
        }

        window.onkeydown = (e: dom.KeyboardEvent) => {
            if (e.keyCode == KeyCode.Space) {
                running = !running
            }
        }

        dom.window.setInterval(
            () => if (running) {
                for (i <- 1 to 10) {
                    currentState.drawing.draw(DrawHtmlAlgebra).apply(ctx)
                    currentState = gen.next(StateEvent(currentState, Unit))
                }
            },
            1
        )
    }

    def initialState(htmlCanvas: dom.html.Canvas, size: Int = 1): GeneratorState =
        GeneratorState(CanvasDrawing.empty, 0)

    def initialState2(htmlCanvas: dom.html.Canvas, size: Int = 1): GeneratorState = GeneratorState(
        new CanvasDrawing {
            override def draw[T](canvasAlgebra: CanvasAlgebra[T]) = canvasAlgebra.sequence(
                canvasAlgebra.fillRect(htmlCanvas.width / 2 - 200, htmlCanvas.height / 2, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2, htmlCanvas.height / 2, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2 + 200, htmlCanvas.height / 2, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2, htmlCanvas.height / 2 + 200, size, size),
                canvasAlgebra.fillRect(htmlCanvas.width / 2, htmlCanvas.height / 2 - 200, size, size)
            )
        },
        0
    )

    def generator(): Generator[Unit] = Generator(
        transition(noiseTransf(3))
    )

    def transition(transf: PlaneTransformation)(se: StateEvent[Unit]): GeneratorState = {
        GeneratorState(
            se.state.drawing.draw(PlaneTransformationAlgebra(transf)),
            se.state.frame + 1
        )
    }

    def noiseTransf(noiseLevel: Double): PlaneTransformation = (point: DoublePoint) => {
        val shift = DoublePoint(
            Random.nextDouble() * 2 * noiseLevel - noiseLevel,
            Random.nextDouble() * 2 * noiseLevel - noiseLevel
        )
        point + shift
    }
}
