package paint

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import paint.Geometry.{DoublePoint, PlaneTransformation, Point}
import paint.canvas._
import paint.canvas.events._
import paint.generator.{Generator, GeneratorState, StateWithEvent}
import paint.html.{NativeRenderingContext, RenderingContext, TransformedCanvasRenderingContext2D}

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
        val canvas = paint.Conf.canvas(ctx)

        ctx.fillStyle = "white"

        val size = 1
        val noise = 1

        val gen = generator(noise, size)
        var currentState  = initialState(htmlCanvas)
        var running = true
        var mouseDown = false


        val iterations = 100
        var previoust: Double = 0
        var lastPrint: Double = 0

        def canvasEvent(event: CanvasEvent) = {
            currentState = gen.next(StateWithEvent(currentState, event))
        }

        def draw(t: Double): Unit = {
            if (currentState.pausedDrawing.isEmpty) {
                //ctx.beginPath()
                for (i <- 1 to iterations) {
                    currentState.drawing.apply(DrawHtmlAlgebra).apply(ctx)
                    canvasEvent(Tick)
                }
                window.requestAnimationFrame(draw)
            }

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
            previoust = t
        }

        htmlCanvas.onmousedown = (e: dom.MouseEvent) => {
            mouseDown = true
            canvasEvent(AddPoint(e.clientX, e.clientY))
        }

        htmlCanvas.onmousemove = (e: dom.MouseEvent) => if (mouseDown) {
            canvasEvent(AddPoint(e.clientX, e.clientY))
        }

        htmlCanvas.onmouseup = (e: dom.MouseEvent) => {
            mouseDown = false
        }

        window.onkeydown = (e: dom.KeyboardEvent) => {
            if (e.keyCode == KeyCode.Space) {
                currentState.pausedDrawing match {
                    case None =>  canvasEvent(Restart)
                    case _ => canvasEvent(Pause)
                }
            }
        }
        dom.window.requestAnimationFrame(draw)
    }

    def initialState(htmlCanvas: dom.html.Canvas, size: Double = 1): GeneratorState =
        GeneratorState(CanvasDrawing.empty, None, 0)

    def generator(noise: Double, size: Double = 1): Generator[CanvasEvent] = {

        def randomizeTransition(generatorState: GeneratorState): GeneratorState = {
            val planeTransf = PlaneTransformationAlgebra(noiseTransf(noise))
            GeneratorState(
                generatorState.drawing.apply(planeTransf),
                None,
                generatorState.frame + 1
            )
        }

        def addPointTransition(x: Double, y: Double, generatorState: GeneratorState): GeneratorState = {
            GeneratorState(
                CanvasDrawing.sequence(
                    generatorState.drawing,
                    CanvasDrawing.fillRect(x, y, size, size)
                ),
                None,
                generatorState.frame + 1
            )
        }

        def pauseTransition(generatorState: GeneratorState) = GeneratorState(
            CanvasDrawing.empty,
            Some(generatorState.drawing),
            generatorState.frame
        )

        def restartTransition(generatorState: GeneratorState) = GeneratorState(
            generatorState.pausedDrawing.getOrElse(CanvasDrawing.empty),
            None,
            generatorState.frame
        )

        new Generator[CanvasEvent] {
            override def next(se: StateWithEvent[CanvasEvent]) = {
                se.event match {
                    case Tick => randomizeTransition(se.state)
                    case Pause => pauseTransition(se.state)
                    case Restart => restartTransition(se.state)
                    case AddPoint(x, y) => addPointTransition(x, y, se.state)
                }
            }
        }
    }


    def noiseTransf(noiseLevel: Double): PlaneTransformation = (point: DoublePoint) => {
        val shift = DoublePoint(
            Random.nextDouble() * 2 - 1,
            Random.nextDouble() * 2 - 1
        ) * noiseLevel
        point + shift
    }
}
