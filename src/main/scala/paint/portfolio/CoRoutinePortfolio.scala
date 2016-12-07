package paint.portfolio

import org.scalajs.dom.CanvasRenderingContext2D
import paint.canvas.CanvasCoRoutine
import paint.coroutine.CoRoutine
import paint.geometry.Geometry.DoublePoint
import paint.geometry.GeometryCoroutine
import paint.random.{NumberCoRoutines, RNG, RandomCoRoutines, SimpleRNG}

import scala.util.Random

/**
  * Created by nic on 06/12/2016.
  */
object CoRoutinePortfolio {
    import GeometryCoroutine._
    import CoRoutine._
    import NumberCoRoutines._
    import RandomCoRoutines._

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
        CanvasCoRoutine.drawPath[Unit](1, point3.sliding(2))
    }

    def test2(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val speed = 2
        val vel1 = const[Unit, DoublePoint](DoublePoint(1, 0) * speed)
        val vel2 = const[Unit, DoublePoint](DoublePoint(0, 1) * speed)

        val velocityCo = RandomCoRoutines.conditionalSwap(
            RandomCoRoutines.bernoulli(0.96, SimpleRNG(Random.nextLong())),
            vel1,
            vel2
        )

        val point = velocity[Unit](
            start,
            velocityCo
        )

        CanvasCoRoutine.drawPath[Unit](2, point.sliding(2))
    }

    def test3(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val speed = 1
        val vel1 = const[Unit, DoublePoint](DoublePoint(1, 0) * speed)
        val vel2 = Rnd.pointInDisk(SimpleRNG(Random.nextLong()), 3)

        val velocityCo = RandomCoRoutines.conditionalSwap(
            RandomCoRoutines.bernoulli(0.8, SimpleRNG(Random.nextLong())),
            vel1,
            vel2
        )

        val point = velocity[Unit](
            start,
            velocityCo
        ).map(_.rounded())

        val point2Rel = radialVelocity(
            DoublePoint(20, 0),
            CoRoutine.const[Unit, Double](0.01)
        )

        val point2 = relative(point, point2Rel)

        CanvasCoRoutine.append(
            CanvasCoRoutine.drawPath[Unit](1, point2.sliding(2)),
            CanvasCoRoutine.drawPath[Unit](1, point.sliding(2))
        )
    }

    def test4Point(start: DoublePoint): CoRoutine[Unit, DoublePoint] = {
        val speed = 1.2

        val rotation = if (Random.nextBoolean()) Math.PI/4 else 0

        val speeds = Vector(
            const[Unit, DoublePoint](DoublePoint(1, 0) * speed),
            const[Unit, DoublePoint](DoublePoint(0, 1) * speed),
            const[Unit, DoublePoint](DoublePoint(-1, 0) * speed),
            const[Unit, DoublePoint](DoublePoint(0, -1) * speed)
        ).map(co => co.map(_.rotate(rotation)))

        val rng1 = SimpleRNG(Random.nextLong())
        val rng2 = SimpleRNG(Random.nextLong())

        val velocityCo = choose[Unit, DoublePoint](
            bernoulli(0.02, rng1),
            limitedInt(rng2, speeds.length).map { speeds(_) },
            speeds(Random.nextInt(speeds.length))
        )

        velocity[Unit](
            start,
            velocityCo
        )
    }

    def test4(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        CanvasCoRoutine.drawPath[Unit](2, test4Point(start).sliding(2))
    }

    def test5(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val pointRel = radialVelocity(
            DoublePoint(10, 0),
            CoRoutine.const[Unit, Double](0.2)
        )

        val point = relative(test4Point(start), pointRel)
        CanvasCoRoutine.drawPath[Unit](2, point.sliding(2))
    }

    def test6(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val rng1 = SimpleRNG(Random.nextLong())
        val rng2 = SimpleRNG(Random.nextLong())
        val rng3 = SimpleRNG(Random.nextLong())
        val speed = 4
        val angle = Math.PI / 4
        val size = 10
        val initialV = DoublePoint(1, 0).rotate(2 * Math.PI * Random.nextDouble())
        val v = initialV * speed
        val velocitySequenceCo = double[Unit](rng1).map(d => v.rotate(d * angle))
        val sizeSequenceCo = double[Unit](rng1).map(_ * size + 1)
        val velAndSize = velocitySequenceCo.zipWith(sizeSequenceCo)

        val velocityAndSizeCo: CoRoutine[Unit, (DoublePoint, Double)] = choose[Unit, (DoublePoint, Double)](
            bernoulli(0.1, rng2),
            velAndSize.map(const),
            const((v, 1))
        )

        val positionCo = velocity(start, velocityAndSizeCo.map(_._1))
        val sizeCo = velocityAndSizeCo.map(_._2)
        CanvasCoRoutine.drawPathWithSize[Unit](sizeCo.zipWith(positionCo.sliding(2)))
    }

    def test7(start: DoublePoint): CoRoutine[Unit, CanvasRenderingContext2D => Unit] = {
        val speed = 3
        val rng = SimpleRNG(Random.nextLong())
        val v = Rnd.pointInDisk(rng, speed)
        CanvasCoRoutine.drawPath(0.5, velocity[Unit](start, v).sliding(2))
    }

    object Rnd {
        import paint.random.NumberCoRoutines._

        def pointInDisk(rng: RNG, radius: Double): CoRoutine[Unit, DoublePoint] = {
            val (rng1, next) = rngs(rng).run(())
            val (rng2, _) = next.run(())
            val normCo = double[Unit](rng1).map( _ * radius)
            val angleCo = double[Unit](rng2).map( _ * 2 * Math.PI)

            val versor = DoublePoint(1, 0)

            normCo.zipWith(angleCo).map { case (norm, angle) =>
                versor.rotate(angle) * norm
            }
        }

        def pointInCircle(rng: RNG, radius: Double): CoRoutine[Unit, DoublePoint] = {
            val versor = DoublePoint(1, 0) * radius
            val angleCo = double[Unit](rng).map( _ * 2 * Math.PI )

            angleCo.map { angle =>
                versor.rotate(angle)
            }
        }
    }
}
