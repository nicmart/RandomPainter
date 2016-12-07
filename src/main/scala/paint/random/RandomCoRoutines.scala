package paint.random

import paint.coroutine.CoRoutine

/**
  * Created by nic on 07/12/2016.
  */
object RandomCoRoutines {

    import NumberCoRoutines._

    def bernoulli(p: Double, rng: RNG): CoRoutine[Unit, Boolean] =
        double(rng).map { _ <= p }

    def conditional[A, B](
        distribution: CoRoutine[Unit, Boolean],
        co1: CoRoutine[A, B],
        co2: CoRoutine[A, B]
    ): CoRoutine[A, B] = CoRoutine { a =>
        val (outcome, nextDistribution) = distribution.run(())
        if (outcome) {
            val (b, nextCo1) = co1.run(a)
            (b, conditional(nextDistribution, nextCo1, co2))
        } else {
            val (b, nextCo2) = co2.run(a)
            (b, conditional(nextDistribution, co1, nextCo2))
        }
    }

    def conditionalSwap[A, B](
        distribution: CoRoutine[Unit, Boolean],
        co1: CoRoutine[A, B],
        co2: CoRoutine[A, B]
    ): CoRoutine[A, B] = CoRoutine { a =>
        val (outcome, nextDistribution) = distribution.run(())
        if (outcome) {
            val (b, nextCo1) = co1.run(a)
            (b, conditionalSwap(nextDistribution, nextCo1, co2))
        } else {
            val (b, nextCo2) = co2.run(a)
            (b, conditionalSwap(nextDistribution, nextCo2, co1))
        }
    }

    def choose[A, B](
        distribution: CoRoutine[Unit, Boolean],
        cosCo: CoRoutine[A, CoRoutine[A, B]],
        currentCo: CoRoutine[A, B]
    ): CoRoutine[A, B] =
        CoRoutine { a =>
            val (change, nextDistribution) = distribution.run(())
            if (change) {
                val (co, nextCosCo) = cosCo.run(a)
                val (b, nextCurrentCo) = co.run(a)
                (b, choose(nextDistribution, nextCosCo, nextCurrentCo))
            } else {
                val (b, nextCurrentCo) = currentCo.run(a)
                (b, choose(nextDistribution, cosCo, nextCurrentCo))
            }
        }
}
