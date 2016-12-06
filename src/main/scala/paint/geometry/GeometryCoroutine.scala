package paint.geometry

import paint.coroutine.CoRoutine
import paint.geometry.Geometry.DoublePoint

/**
  * Created by nic on 06/12/2016.
  */
object GeometryCoroutine {

    def position[A](co1: CoRoutine[A, Double], co2: CoRoutine[A, Double]): CoRoutine[A, DoublePoint] =
        co1.zipWith(co2).map { case (x, y) => DoublePoint(x, y) }

    /**
      * Translate the evolution described by pointCo using the coordinates given by centerCo
      */
    def relative[A](centerCo: CoRoutine[A, DoublePoint], pointCo: CoRoutine[A, DoublePoint]): CoRoutine[A, DoublePoint] =
        CoRoutine { a =>
            val (centerPosition, nextCenterCo) = centerCo.run(a)
            val (pointPosition, nextPointCo) = pointCo.run(a)
            (centerPosition + pointPosition, relative(nextCenterCo, nextPointCo))
        }

    /**
      * Evolution of a point determined by the evolution of its velocity
      */
    def velocity[A](start: DoublePoint, velocityCo: CoRoutine[A, DoublePoint]): CoRoutine[A, DoublePoint] =
        CoRoutine { a =>
            val (v, nextVelocityCo) = velocityCo.run(a)
            (start, velocity(start + v, nextVelocityCo))
        }


    /**
      * Evolution of a point determined by the evolution of its acceleration.
      * From the implementation you can see how the acceleration is the speed of speed...
      */
    def acceleration[A](
        start: DoublePoint,
        v: DoublePoint,
        accelerationCo: CoRoutine[A, DoublePoint]
    ): CoRoutine[A, DoublePoint] =
        velocity(start, velocity(v, accelerationCo))
}
