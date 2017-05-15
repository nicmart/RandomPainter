package paint.evolution

import paint.random.RNG

import scala.collection.immutable.{Queue, Stream}

/**
  * Created by Nicolò Martini on 12/05/2017.
  */
case class Evolution[A](run: RNG => (RNG, A, Evolution[A])) {

    def mapEv[B](f: (A, Evolution[A]) => (B, Evolution[B])): Evolution[B] =
        Evolution { rng =>
            val (rng2, a, eva2) = run(rng)
            val (b, evb2) = f(a, eva2)
            (rng2, b, evb2)
        }

    def mapEv[B](f: A => B, g: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
        mapEv((a, eva) => (f(a), g(a, eva)))

    def map[B](f: A => B): Evolution[B] =
        mapEv(f, (_, eva) => eva.map(f))

    def compose[B, C](evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
        Evolution.map2(f)(this, evb)

    def flatNext[B](f: A => Evolution[B])(g: (Evolution[A], Evolution[B]) => Evolution[B]): Evolution[B] =
        Evolution { rng =>
            val (rng2, a, eva2) = run(rng)
            val (rng3, b, evb2) = f(a).run(rng2)
            (rng3, b, g(eva2, evb2))
        }

    def flatMap[B](f: A => Evolution[B]): Evolution[B] =
        flatNext(f)((eva2, _ ) => eva2.flatMap(f))

    def replace[B](f: A => Evolution[B]): Evolution[B] =
        flatNext(f)((_, evb) => evb)

    def scan[Z](f: (Z, A) => Z)(z: Z): Evolution[Z] =
        mapEv(f(z, _), (a, eva) => eva.scan(f)(f(z, a)))

    def prepend(as: List[A]): Evolution[A] = Evolution { rng =>
        as match {
            case Nil => run(rng)
            case head :: tail => (rng, head, prepend(tail))
        }
    }

    def prepen2d(as: List[A]): Evolution[A] = as match {
        case Nil => this
        case head :: tail =>
            mapEv(_ => head, (_, _) => prepend(tail))
    }

    def tail: Evolution[A] = Evolution { rng =>
        run(rng)._3.run(rng)
    }

    def drop(n: Int): Evolution[A] = n match {
        case _ if n <= 0 => this
        case _ => tail.drop(n - 1)
    }

    def speedUp(skip: Int): Evolution[A] =
        mapEv(a => a, (_, eva) => eva.drop(skip).speedUp(skip))

    def slowDown(times: Int): Evolution[A] =
        Evolution.flatten(map(List.fill(times)(_)))

    def unfold(from: RNG): Stream[A] = {
        val (rng2, a, ev2) = run(from)
        a #:: ev2.unfold(rng2)
    }
}

object Evolution {
    def pure[A](value: A): Evolution[A] =
        Evolution { (_, value , pure(value)) }

    def map2[A, B, C](f: (A, B) => C)(eva: Evolution[A], evb: Evolution[B]): Evolution[C] =
        Evolution { rng =>
            val (rng2, a, eva2) = eva.run(rng)
            val (rng3, b, evb2) = evb.run(rng2)
            (rng3, f(a, b), map2(f)(eva2, evb2))
        }

    def traverse[A, B](as: List[A])(f: A => Evolution[B]): Evolution[List[B]] =
        as.foldRight[Evolution[List[B]]](pure(List())) { (a, evb) =>
            f(a).compose(evb)(_ :: _)
        }

    def sequence[A](evolutions: List[Evolution[A]]): Evolution[List[A]] =
        traverse(evolutions)(ev => ev)

    def prepend[A](as: List[A])(eva: => Evolution[A]): Evolution[A] = Evolution { rng =>
        as match {
            case Nil => eva.run(rng)
            case head :: tail => (rng, head, prepend(tail)(eva))
        }
    }

    def cycle[A](as: List[A]): Evolution[A] = prepend(as)(cycle(as))

    def flatten[A](evas: Evolution[List[A]]): Evolution[A] = {
        evas.replace { as =>
            prepend(as)(flatten(evas.tail))
        }
    }
}
