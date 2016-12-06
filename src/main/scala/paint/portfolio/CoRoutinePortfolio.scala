package paint.portfolio

import org.scalajs.dom.CanvasRenderingContext2D
import paint.canvas.CanvasCoRoutine
import paint.coroutine.CoRoutine
import paint.geometry.Geometry.DoublePoint
import paint.geometry.GeometryCoroutine
import paint.random.{RNG, SimpleRNG}

import scala.util.Random

/**
  * Created by nic on 06/12/2016.
  */
object CoRoutinePortfolio {
    import GeometryCoroutine._
    import CoRoutine._

    def test(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val vel = const[Unit, DoublePoint](DoublePoint(1, 0))
        //val point = GeometryCoroutine.velocity(start, velocity)
        val point = velocity[Unit](
            start,
            Rnd.pointInCircle(SimpleRNG(Random.nextLong()), 2)
        )
        val point2Rel = radialVelocity(
            DoublePoint(30, 0),
            CoRoutine.const[Unit, Double](0.01)
        )

        val point2 = relative(point, point2Rel)

        val point3Rel = radialVelocity(
            DoublePoint(10, 0),
            CoRoutine.const[Unit, Double](0.2)
        )

        val point3 = relative(point2, point3Rel)
        CanvasCoRoutine.drawPoint(1, point3)
    }

    def test2(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val vel = const[Unit, DoublePoint](DoublePoint(1, 0))
        //val point = GeometryCoroutine.velocity(start, velocity)
        val point = velocity[Unit](
            start,
            Rnd.pointInCircle(SimpleRNG(Random.nextLong()), 2)
        )
        val point2Rel = radialVelocity(
            DoublePoint(30, 0),
            CoRoutine.const[Unit, Double](0.01)
        )

        val point2 = relative(point, point2Rel)

        val point3Rel = radialVelocity(
            DoublePoint(10, 0),
            CoRoutine.const[Unit, Double](0.2)
        )

        val point3 = relative(point2, point3Rel)
        CanvasCoRoutine.drawPoint(1, point3)
    }

    object Rnd {
        import paint.random.NumberCoRoutines._

        def pointInDisk(rng: RNG, radius: Double): CoRoutine[Unit, DoublePoint] = {
            val (rng1, next) = rngs(rng).run(())
            val (rng2, _) = next.run(())
            val normCo = double(rng1).map( _ * radius)
            val angleCo = double(rng2).map( _ * 2 * Math.PI)

            val versor = DoublePoint(1, 0)

            normCo.zipWith(angleCo).map { case (norm, angle) =>
                versor.rotate(angle) * norm
            }
        }

        def pointInCircle(rng: RNG, radius: Double): CoRoutine[Unit, DoublePoint] = {
            val versor = DoublePoint(1, 0) * radius
            val angleCo = double(rng).map( _ * 2 * Math.PI )

            angleCo.map { angle =>
                versor.rotate(angle)
            }
        }
    }
}
