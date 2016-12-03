package paint.generator

import paint.Canvas.CanvasAction
import paint.canvas.CanvasDrawing

case class GeneratorState(
    drawing: CanvasDrawing,
    pausedDrawing: Option[CanvasDrawing],
    frame: Int
) {
    def withDrawing(newDrawing: CanvasDrawing): GeneratorState = GeneratorState(
        newDrawing,
        pausedDrawing,
        frame
    )
}

case class StateWithEvent[E](state: GeneratorState, event: E)

trait Generator[E] {
    def next(se: StateWithEvent[E]): GeneratorState
}

object Generator {
    def apply[C, E](f: StateWithEvent[E] => GeneratorState) = new Generator[E] {
        override def next(se: StateWithEvent[E]): GeneratorState  = {
            f(se)
        }
    }
}