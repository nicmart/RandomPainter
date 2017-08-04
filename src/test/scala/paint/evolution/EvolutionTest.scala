package paint.evolution

import org.scalatest.{FlatSpec, Matchers, WordSpec}
import Evolution._
import Numeric._
import org.scalatest._

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
class EvolutionTest extends PaintEvolutionSpec {

    "A slowed down evolution" must {
        "return the same evolution if slowed down by 1" in {
            1 shouldBe 2
            evolutionEquivalent(int, int.slowDown(1))
        }
    }
}
