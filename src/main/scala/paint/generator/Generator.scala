package paint.generator

import paint.Canvas.CanvasAction
import paint.canvas.CanvasDrawing

case class GeneratorState(
    drawing: CanvasDrawing,
    frame: Int
)

case class StateEvent[E](state: GeneratorState, event: E)

trait Generator[E] {
    def next(se: StateEvent[E]): GeneratorState
}

object Generator {
    def apply[C, E](f: StateEvent[E] => GeneratorState) = new Generator[E] {
        override def next(se: StateEvent[E]): GeneratorState  = f(se)
    }
}