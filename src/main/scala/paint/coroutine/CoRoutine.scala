package paint.coroutine

/**
  * Created by nic on 05/12/2016.
  */
case class CoRoutine[A, B](run: A => (B, CoRoutine[A,B])) {

    def map[C](f: B => C): CoRoutine[A, C] =
        CoRoutine { a =>
            val (b, next) = run(a)
            (f(b), next.map(f))
        }

    def andThen[C](co: CoRoutine[B, C]): CoRoutine[A, C] =
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

    def zipWith[C](co: CoRoutine[A, C]): CoRoutine[A, (B, C)] =
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

    def sliding(n: Int, bs: Vector[B] = Vector()): CoRoutine[A, Vector[B]] =
        CoRoutine { a =>
            val (b, nextCo) = run(a)
            val nextBs = bs.takeRight(n - 1) :+ b
            (nextBs, nextCo.sliding(n, nextBs))
        }
}

object CoRoutine {

    def func[A, B](f: A => B): CoRoutine[A, B] =
        CoRoutine { a =>
            (f(a), func(f))
        }

    def const[A, B](value: B): CoRoutine[A, B] = func { _ => value }

    def id[A]: CoRoutine[A, A] = func(identity)

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
}
