package paint.generator

import paint.Canvas.CanvasAction
import paint.canvas.CanvasDrawing

case class GeneratorState(
    drawing: CanvasDrawing,
    frame: Int
) {
    def withDrawing(newDrawing: CanvasDrawing): GeneratorState = GeneratorState(
        newDrawing,
        frame
    )
}

case class StateEvent[E](state: GeneratorState, event: E)

trait Generator[E] {
    def next(se: StateEvent[E]): GeneratorState
}

object Generator {
    def apply[C, E](f: StateEvent[E] => GeneratorState) = new Generator[E] {
        override def next(se: StateEvent[E]): GeneratorState  = f(se)
    }
}