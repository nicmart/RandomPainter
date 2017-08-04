import org.scalatest.{Matchers, WordSpec}
import paint.random.SequenceRNG
import org.scalatest.{Matchers, WordSpec}
import paint.evolution.Evolution
import paint.evolution.Evolution._
import paint.random.SequenceRNG

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
trait PaintEvolutionSpec extends WordSpec with Matchers {
    def evolutionEquivalent[A](ev1: Evolution[A], ev2: Evolution[A]): Unit = {
        val rng = SequenceRNG(0)
        val sample1 = debug(ev1, 50)(Some(rng))
        val sample2 = debug(ev2, 50)(Some(rng))
        sample1 shouldBe sample2
    }
}
