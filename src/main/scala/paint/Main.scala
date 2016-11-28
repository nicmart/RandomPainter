package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import paint.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.generator.{Generator, GeneratorState, StateEvent, Generator$}
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
        var currentState = initialState(htmlCanvas)

        dom.window.setInterval(
            () => {
                val action = currentState.action
                canvas(action)
                currentState = gen.next(StateEvent(currentState, Unit))
            },
            100
        )
    }

    def initialState(htmlCanvas: dom.html.Canvas): GeneratorState[RenderingContext] =
        GeneratorState[RenderingContext](
            Canvas.drawSequence[RenderingContext](
                Canvas.drawPoint(Point(htmlCanvas.width / 2, htmlCanvas.height / 2), 10),
                Canvas.drawPoint(Point(htmlCanvas.width / 2 + 200, htmlCanvas.height / 2), 10),
                Canvas.drawPoint(Point(htmlCanvas.width / 2 - 200, htmlCanvas.height / 2), 10)
            ),
            0
        )

    def generator(): Generator[RenderingContext, Unit] = Generator(
        transition(noiseTransf(1))
    )

    def transition(transf: PlaneTransformation)(se: StateEvent[RenderingContext, Unit]): GeneratorState[RenderingContext] = {
        GeneratorState(
            Canvas.drawTransformed(transf)(se.state.action),
            se.state.frame + 1
        )
    }

    def noiseTransf(noiseLevel: Int): PlaneTransformation = (point: DoublePoint) => {
        val shift = DoublePoint(
            Random.nextInt(2 * noiseLevel + 1) - noiseLevel,
            Random.nextInt(2 * noiseLevel + 1) - noiseLevel
        )
        point + shift
    }

    def frame(htmlCanvas: dom.html.Canvas): Seq[Canvas.CanvasAction[RenderingContext]] = {
        val ctx = htmlCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]
        paint.Conf.canvasInitializer.initialise(htmlCanvas)
        val canvas = paint.Conf.canvas(ctx)

        ctx.fillStyle = "white"
        val action1 = Canvas.drawSequence[RenderingContext](
            Canvas.drawPoint(Point(htmlCanvas.width / 2, htmlCanvas.height / 2), 10),
            Canvas.drawPoint(Point(htmlCanvas.width / 2 + 200, htmlCanvas.height / 2), 10),
            Canvas.drawPoint(Point(htmlCanvas.width / 2 - 200, htmlCanvas.height / 2), 10)
            //Canvas.drawPoint(Point(htmlCanvas.width / 2, htmlCanvas.height / 2 + 100), 10)
        )

        val transformation: PlaneTransformation = (point: DoublePoint) => point / 2

        def noiseTransf(noiseLevel: Int): PlaneTransformation = (point: DoublePoint) => {
            val radius = noiseLevel / 2
            val shift = DoublePoint(
                Random.nextInt(noiseLevel) - radius,
                Random.nextInt(noiseLevel) - radius
            )
            point + shift
        }

        //val noiser = Canvas.drawAndTransform(noiseTransf(100)) _
        val noiser = Canvas.drawTransformed(noiseTransf(10)) _

        var action = Canvas.drawNothing
        var last = action1

        for (i <- 1 to 1000) yield {
            last = noiser(last)
            last
        }
    }
}
