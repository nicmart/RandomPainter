package paint.random

import paint.coroutine.CoRoutine

/**
  * Created by nic on 06/12/2016.
  */
object NumberCoRoutines {

    def int[A](rng: RNG): CoRoutine[A, Int] =
        CoRoutine { _ =>
            val (integer, nextRng) = rng.nextInt
            (integer, int(nextRng))
        }

    def limitedInt[A](rng: RNG, max: Int): CoRoutine[A, Int] =
        double(rng).map { d => (max * d).toInt }

    def double[A](rng: RNG): CoRoutine[A, Double] =
        CoRoutine { _ =>
            val (integer, nextRng) = rng.nextInt
            val d = (integer.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue)
            (d, double(nextRng))
        }

    def long[A](rng: RNG): CoRoutine[A, Long] =
        int(rng).pairs().map { case (n, m) =>
            (n.toLong << 32) + m
        }

    def rngs[A](rng: RNG): CoRoutine[A, RNG] =
        long(rng).map(SimpleRNG)
}
