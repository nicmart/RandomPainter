package paint.coroutine

import cats.Semigroup
import cats.syntax.semigroup._

/**
  * Created by nic on 05/12/2016.
  */
case class CoRoutine[-A, +B](run: A => (B, CoRoutine[A,B])) {

    def map[C](f: B => C): CoRoutine[A, C] =
        CoRoutine { a =>
            val (b, next) = run(a)
            (f(b), next.map(f))
        }

    def flatMap[AA <: A, C](f: B => CoRoutine[AA, C]): CoRoutine[AA, C] = CoRoutine { aa =>
        val (b, co2) = run(aa)
        val (c, co3) = f(b).run(aa)
        (c, co2.flatMap(f))
    }

    def mapp[C](f: B => C): CoRoutine[A, C] =
        flatMap(b => CoRoutine.const[A, C](f(b)))

    /**
      * {{
      * flatMap(b => CoRoutine.const[A, C](f(b)))
      * CoRoutine { aa =>
            val (b, co2) = run(aa)
            val (c, co3) = CoRoutine.const[A, C](f(b)).run(aa)
            (c, co2.flatMap(b => CoRoutine.const[A, C](f(b))))
        }
      * CoRoutine { aa =>
            val (b, co2) = run(aa)
            val (c, co3) = (f(b), const...)
            (c, co2.flatMap(b => CoRoutine.const[A, C](f(b))))
        }
      * CoRoutine { aa =>
            val (b, co2) = run(aa)
            val (c, co3) = (f(b), const...)
            (f(b), co2.map(f))
        }
      * }}
      */

    def andThen[C >: B, D](co: CoRoutine[C, D]): CoRoutine[A, D] =
        CoRoutine { a =>
            val (b, next) = run(a)
            val (c, nextCo) = co.run(b)
            (c, next.andThen(nextCo))
        }

    def zip[C, D](co: CoRoutine[C, D]): CoRoutine[(A, C), (B, D)] =
        CoRoutine { case (a, c) =>
            val (b, next1) = run(a)
            val (d, next2) = co.run(c)
            ((b, d), next1 zip next2)
        }

    def zipWith[C <: A, D](co: CoRoutine[C, D]): CoRoutine[C, (B, D)] =
        CoRoutine { a =>
            val (b, ab) = run(a)
            val (c, ac) = co.run(a)
            ((b, c), ab.zipWith(ac))
        }

    def tail(): CoRoutine[A, B] =
        CoRoutine { a =>
            run(a)._2.run(a)
        }

    def pairs(): CoRoutine[A, (B, B)] =
        CoRoutine { a =>
            val (b1, next1Co) = run(a)
            val (b2, next2Co) = next1Co.run(a)
            ((b1, b2), next2Co.pairs())
        }

    def sliding[C >: B](n: Int, bs: Vector[C] = Vector()): CoRoutine[A, Vector[C]] =
        CoRoutine { a =>
            val (b, nextCo) = run(a)
            val nextBs = bs.takeRight(n - 1) :+ b
            (nextBs, nextCo.sliding(n, nextBs))
        }

    //def grouped(n: Int):
}

object CoRoutine {

    def func[A, B](f: A => B): CoRoutine[A, B] =
        CoRoutine { a =>
            (f(a), func(f))
        }

    def const[A, B](value: B): CoRoutine[A, B] = func { _ => value }

    def id[A]: CoRoutine[A, A] = func(identity)

    def map2[A, B, C, D](cob: CoRoutine[A, B], coc: CoRoutine[A, C])(f: (B, C) => D): CoRoutine[A, D] =
        CoRoutine { a =>
            val (b, nextCob) = cob.run(a)
            val (c, nextCoc) = coc.run(a)
            (f(b, c), map2(nextCob, nextCoc)(f))
        }

    def traverse[A, B, C](as: List[B])(f: B => CoRoutine[A, C]): CoRoutine[A, List[C]] =
        as.foldRight(const[A, List[C]](List[C]()))((b, cbs) => map2(f(b), cbs)(_ :: _))

    def sequence[A, B](cos: List[CoRoutine[A, B]]): CoRoutine[A, List[B]] =
        traverse(cos)(identity)

    def fold[A, B](f: (A, B) => B)(start: B): CoRoutine[A, B] =
        CoRoutine { a =>
            val next = f(a, start)
            (next, fold(f)(next))
        }

    def foldMap[A, B, Z](co: CoRoutine[A, B])(z: Z)(f: (B, Z) => Z): CoRoutine[A, Z] =
        CoRoutine { a =>
            val (b, nextCo) = co.run(a)
            val zNext = f(b, z)
            (zNext, foldMap(nextCo)(zNext)(f))
        }

    def first[A, B, C](co: CoRoutine[A, B]): CoRoutine[(A, C), (B, C)] =
        co.zip(id)

    def second[A, B, C](co: CoRoutine[A, B]): CoRoutine[(C, A), (C, B)] =
        id.zip(co)

    def toStream[A](start: A, co: CoRoutine[A, A]): Stream[A] = {
        val (second, next) = co.run(start)
        start #:: toStream(second, next)
    }

    def toStream[A](co: CoRoutine[Unit, A]): Stream[A] = {
        val (a, next) = co.run(())
        a #:: toStream(next)
    }

    def fromStream[A](stream: Stream[A]): CoRoutine[Unit, A] =
        CoRoutine { _ =>
            (stream.head, fromStream(stream.tail))
        }

    def integrate[A, B: Semigroup](derivativeCo: CoRoutine[A, B])(start: B): CoRoutine[A, B] =
        CoRoutine { a =>
            val (b, nextDerivativeCo) = derivativeCo.run(a)
            (start, integrate(nextDerivativeCo)(start |+| b))
        }
}
