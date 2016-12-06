package paint.generative

import paint.geometry.Geometry.DoublePoint
import paint.algebra.{PaintDrawing, Point}

import scala.util.Random

/**
  * Created by nic on 04/12/2016.
  */
object StateTransformation {

    def brownianTransformation(strength: Double): Point => PaintDrawing = { p =>
        val velocity = DoublePoint(strength, 0).rotate(2 * Math.PI * Random.nextDouble())
        PaintDrawing.point(
            p.position + velocity,
            velocity,
            p.size
        )
    }

    def brownianVelocityTransformation(strength: Double): Point => PaintDrawing = { p =>
        val velocityAlteration = DoublePoint(strength, 0).rotate(2 * Math.PI * Random.nextDouble())
        val newVelocity = p.velocity + velocityAlteration
        PaintDrawing.point(
            p.position + newVelocity,
            newVelocity,
            p.size
        )
    }

    def relativeBrownianVelocityTransformation(strength: Double): Point => PaintDrawing = { p =>
        val velocityAlteration = DoublePoint(1, 0).rotate(2 * Math.PI * Random.nextDouble()) * strength * p.velocity.norm()
        val newVelocity = p.velocity + velocityAlteration
        PaintDrawing.point(
            p.position + newVelocity,
            newVelocity,
            p.size
        )
    }

    def withSpeedLimit(limit: Double): Point => PaintDrawing = { point => {
        val speed = point.velocity.norm()
        if (speed > limit) {
            val newVelocity = point.velocity * (limit / speed)
            PaintDrawing.point(
                point.position,
                newVelocity,
                point.size
            )
        } else {
            PaintDrawing.point(
                point
            )
        }
    }}

    def withSpeedMin(min: Double): Point => PaintDrawing = { point => {
        val speed = point.velocity.norm()
        if (speed < min) {
            val newVelocity = point.velocity * (min / speed)
            PaintDrawing.point(
                point.position,
                newVelocity,
                point.size
            )
        } else {
            PaintDrawing.point(
                point
            )
        }
    }}

    def withProbability(
        p: Double,
        transformation: Point => PaintDrawing,
        elseTransformation: Option[Point => PaintDrawing] = None
    ): Point => PaintDrawing = {
        point => {
            if (Random.nextDouble() < p) {
                transformation(point)
            } else {
                elseTransformation.map(_(point)).getOrElse(PaintDrawing.point(point))
            }
        }
    }

    def withGravity(
        center: DoublePoint,
        constant: Double
    ): Point => PaintDrawing = { point =>
        val versor = (center - point.position).versor().get
        val acceleration =  constant * 1 / Math.pow(point.position.distance(center), 2)
        val gravity = versor * (acceleration * constant)
        val newVelocity = point.velocity + gravity
        PaintDrawing.point(point.copy(velocity = newVelocity))
    }

    def sizeFloat(strength: Double, min: Double, max: Double): Point => PaintDrawing = { p => {
        val newSize = Math.max(min, Math.min(max, (1 + (Random.nextDouble() - 0.5) * strength) * p.size))
        PaintDrawing.point(p.position, p.velocity, newSize)
    }}

    def sizeFromSpeed(f: Double => Double): Point => PaintDrawing = { p => {
        PaintDrawing.point(p.position, p.velocity, f(p.velocity.norm()))
    }}

    def branch(angle: Double, sizeFactor: Double, sizeLimit: Double): Point => PaintDrawing = { p => {
        val newSize = p.size * sizeFactor
        if (newSize < sizeLimit) {
            PaintDrawing.point(p)
        } else {

            val velocity1 = p.velocity.rotate (- angle)
            val velocity2 = p.velocity.rotate (angle)

            PaintDrawing.sequence(List(
                PaintDrawing.point(
                    p.position,
                    velocity1,
                    p.size * sizeFactor
                ),
                PaintDrawing.point (
                    p.position,
                    velocity2,
                    p.size * sizeFactor
                )
            ))
        }
    }}

    def removePointsSmallerThan(size: Double): Point => PaintDrawing = { p =>
        if (p.size < size) PaintDrawing.empty else PaintDrawing.point(p)
    }
}
