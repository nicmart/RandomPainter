package paint.portfolio

import paint.geometry.Geometry.DoublePoint
import paint.algebra.{DrawOnCanvasPaintAlgebra, LineDrawOnCanvasPaintAlgebra, PaintDrawing}
import paint.events.{AddPoint, CanvasEvent}
import paint.generative._

import scala.util.Random

/**
  * Created by nic on 04/12/2016.
  */
case class Portfolio(canvasSize: DoublePoint) {

    lazy val brownianClouds: Drawing[CanvasEvent] = {
        val size = 0.5
        val noise = 1
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianTransformation(noise)),
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            DrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianSoftClouds: Drawing[CanvasEvent] = {
        val size = 0.1
        val noise = 3
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianTransformation(noise)),
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            DrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianDust2: Drawing[CanvasEvent] = {
        val size = 0.5
        val noise = 4
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianTransformation(noise)),
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianLines: Drawing[CanvasEvent] = {
        val size = 0.5
        val noise = 10
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianTransformation(noise)),
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianVelocity: Drawing[CanvasEvent] = {
        val size = 1
        val noise = 2
        val speedLimit = 10
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianVelocityTransformation(noise))
                .flatMap(StateTransformation.withSpeedLimit(speedLimit))
            ,
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianVelocityWithRelativeSpeedAlteration: Drawing[CanvasEvent] = {
        val size = 1
        val noise = 0.5
        val speedLimit = 4
        val initialVelocity = DoublePoint(0.1, 0)

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.relativeBrownianVelocityTransformation(noise))
            ,
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val sizeFloat: Drawing[CanvasEvent] = {
        val size = 0.5
        val noise = 0.05
        val speedLimit = 10
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianVelocityTransformation(noise))
                .flatMap(StateTransformation.withSpeedLimit(speedLimit))
                .flatMap(StateTransformation.sizeFloat(0.4, 0.3, 20))
            ,
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val gravity: Drawing[CanvasEvent] = {
        val size = 1
        val noise = 0.1
        val speedLimit = 10
        val gravityCenter = canvasSize / 2
        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianVelocityTransformation(noise))
                .flatMap(StateTransformation.withGravity(gravityCenter, 20))
                //.flatMap(StateTransformation.withSpeedLimit(speedLimit))
            ,
            (position: DoublePoint) => {
                val initialVelocity = (gravityCenter - position)
                    .versor()
                    .get.rotate(Math.PI / 2 + (Random.nextDouble() - 1/2) * 0.5)
                AddPoint(position, initialVelocity, size)
            },
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianMadnessSizeFloat: Drawing[CanvasEvent] = {
        val size = 3
        val noise1 = 10
        val noise2 = 0.2
        val probability = 0.1
        val speedLimit = 30
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(
                StateTransformation.withProbability(
                    probability,
                    StateTransformation.brownianVelocityTransformation(noise1),
                    Some(StateTransformation.brownianTransformation(noise2))
                )
            )
                .flatMap(StateTransformation.withSpeedLimit(speedLimit))
            //    .flatMap(StateTransformation.sizeFloat(0.4, 0.5, 5))
                .flatMap(StateTransformation.sizeFromSpeed(0.05 * Math.pow(_, 1.9)))
            ,
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val brownianMadness: Drawing[CanvasEvent] = {
        val size = 3
        val noise1 = 10
        val noise2 = 0.2
        val probability = 0.1
        val speedLimit = 30
        val initialVelocity = DoublePoint.zero

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(
                StateTransformation.withProbability(
                    probability,
                    StateTransformation.brownianVelocityTransformation(noise1),
                    Some(StateTransformation.brownianTransformation(noise2))
                )
            )
                .flatMap(StateTransformation.withSpeedLimit(speedLimit))
            ,
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }

    lazy val trees: Drawing[CanvasEvent] = {

        val size = 30
        val noise = 0.15
        val initialVelocity = DoublePoint(0, -3)
        val branchingProbability = 0.04
        val branchingAngle = 0.4
        val branchingSizeFactor = 0.6
        val branchingSizeLimit = 0.5
        val removeSmallerThan = 1.0
        val speedLimit = 3

        TransitionDrawing[CanvasEvent](
            State.empty,
            DefaultInteractionStateTransition(StateTransformation.brownianVelocityTransformation(noise))
                .flatMap(StateTransformation.withProbability(
                    branchingProbability,
                    StateTransformation.branch(branchingAngle, branchingSizeFactor, branchingSizeLimit)
                ))
                .flatMap(StateTransformation.removePointsSmallerThan(removeSmallerThan))
                .flatMap(StateTransformation.withSpeedLimit(speedLimit))
            ,
            (position: DoublePoint) => AddPoint(position, initialVelocity, size),
            LineDrawOnCanvasPaintAlgebra
        )
    }
}
