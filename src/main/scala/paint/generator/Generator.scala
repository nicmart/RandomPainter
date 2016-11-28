package paint.generator

import paint.Canvas.CanvasAction

case class GeneratorState[C](
    action: CanvasAction[C],
    frame: Int
)

case class StateEvent[C, E](state: GeneratorState[C], event: E)

trait Generator[C, E] {
    def next(se: StateEvent[C, E]): GeneratorState[C]
}

object Generator {
    def apply[C, E](f: StateEvent[C, E] => GeneratorState[C]) = new Generator[C, E] {
        override def next(se: StateEvent[C, E]): GeneratorState[C]  = f(se)
    }
}