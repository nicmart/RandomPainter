package paint.random

import paint.coroutine.CoRoutine

/**
  * Created by nic on 06/12/2016.
  */
object NumberCoRoutines {

    type CoDouble[A] = CoRoutine[A, Double]
    type CoInt[A] = CoRoutine[A, Int]
    type CoLong[A] = CoRoutine[A, Long]

    def int[A](rng: RNG): CoInt[A] =
        CoRoutine { _ =>
            val (integer, nextRng) = rng.nextInt
            (integer, int(nextRng))
        }

    def limitedInt[A](rng: RNG, max: Int): CoInt[A] =
        double(rng).map { d => (max * d).toInt }

    def double[A](rng: RNG): CoDouble[A] =
        CoRoutine { _ =>
            val (integer, nextRng) = rng.nextInt
            val d = (integer.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue)
            (d, double(nextRng))
        }

    def long[A](rng: RNG): CoLong[A] =
        int(rng).pairs().map { case (n, m) =>
            (n.toLong << 32) + m
        }

    def rngs[A](rng: RNG): CoRoutine[A, RNG] =
        long(rng).map(SimpleRNG)

    def normal[A](rng: RNG): CoDouble[A] = {
        def normalDoubles(doubles: CoRoutine[A, Double]): CoRoutine[A, Double] =
            CoRoutine { a =>
                var doublesVar: CoRoutine[A, Double] = doubles
                var v1 = .0
                var v2 = .0
                var s = .0
                do {
                    val (d1, doublesVar1) = doublesVar.run(a)
                    var (d2, doublesVar2) = doublesVar1.run(a)
                    doublesVar = doublesVar2
                    v1 = 2 * d1 - 1 // between -1 and 1
                    v2 = 2 * d2 - 1
                    s = v1 * v1 + v2 * v2
                } while (s >= 1 || s == 0)
                val multiplier: Double = Math.sqrt(-2 * Math.log(s) / s)
                (v1 * multiplier, normalDoubles(doublesVar))
            }

        normalDoubles(double(rng))
    }

    def normal[A](rng: RNG, mean: Double, stdDev: Double): CoDouble[A] =
        normal(rng).map(x => (stdDev * x) + mean)

    def boundedDouble1[A](strength: Double, radius: Double, center: Double, current: Double)(doubles: CoDouble[A]): CoDouble[A] =
        CoRoutine { a =>
            val (d, nextDoubles) = doubles.run(a)
            val variation = (d * 2 - 1) * strength * (1 - Math.abs(center - current) / radius)
            (
                current + variation,
                boundedDouble1(strength, radius, center, current + variation)(nextDoubles)
            )
        }

    def boundedDouble2[A](strength: Double, radius: Double, center: Double, current: Double)(doubles: CoDouble[A]): CoDouble[A] =
        CoRoutine { a =>
            val (d, nextDoubles) = doubles.run(a)
            val variation = (d * 2 - 1) * strength
            val newCurrent = if (Math.abs(current + variation - center) > radius){
                current
            } else {
                current + variation
            }
            (
                newCurrent,
                boundedDouble2(strength, radius, center, newCurrent)(nextDoubles)
            )
        }
}
