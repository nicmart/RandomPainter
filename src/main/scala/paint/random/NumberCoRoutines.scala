package paint.random

import paint.coroutine.CoRoutine

/**
  * Created by nic on 06/12/2016.
  */
object NumberCoRoutines {

    def int(rng: RNG): CoRoutine[Unit, Int] =
        CoRoutine { _ =>
            val (integer, nextRng) = rng.nextInt
            (integer, int(nextRng))
        }

    def limitedInt(rng: RNG, max: Int): CoRoutine[Unit, Int] =
        double(rng).map { d => (max * d).toInt }

    def double(rng: RNG): CoRoutine[Unit, Double] =
        CoRoutine { _ =>
            val (integer, nextRng) = rng.nextInt
            val d = (integer.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue)
            (d, double(nextRng))
        }

    def long(rng: RNG): CoRoutine[Unit, Long] =
        int(rng).pairs().map { case (n, m) =>
            (n.toLong << 32) + m
        }

    def rngs(rng: RNG): CoRoutine[Unit, RNG] =
        long(rng).map(SimpleRNG)
}
