import paint.evolution.Numeric._

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
