package paint.generative

import paint.Canvas.CanvasAction
import paint.algebra.PaintDrawing
import paint.canvas.CanvasDrawing

case class State(
    drawing: PaintDrawing,
    pausedDrawing: Option[PaintDrawing]
) {
    def withDrawing(newDrawing: PaintDrawing): State = State(
        newDrawing,
        pausedDrawing
    )

    def pause(): State = State(
        PaintDrawing.empty,
        Some(drawing)
    )

    def restart(): State = State(
        pausedDrawing.getOrElse(drawing),
        None
    )

    def isRunning: Boolean = pausedDrawing.isEmpty

    def toggle(): State = if (isRunning) pause() else restart()
}

object State {
    val empty = State(PaintDrawing.empty, None)
}

case class StateWithEvent[E](state: State, event: E)