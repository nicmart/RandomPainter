package paint.generative

import paint.algebra.{PaintDrawing, Point, PointTransformationAlgebra}
import paint.events._

trait StateTransition[E] {
    def next(se: StateWithEvent[E]): State
}

object StateTransition {
    def apply[E](f: StateWithEvent[E] => State) = new StateTransition[E] {
        override def next(se: StateWithEvent[E]): State  = {
            f(se)
        }
    }
}

case class DefaultInteractionStateTransition(f: Point => PaintDrawing) extends StateTransition[CanvasEvent] {
    override def next(se: StateWithEvent[CanvasEvent]): State = se.event match {
        case Tick => se.state.withDrawing(
            se.state.drawing(PointTransformationAlgebra(f))
        )
        case AddPoint(position, velocity, size) => State(
            PaintDrawing.sequence(List(
                se.state.drawing,
                PaintDrawing.point(position, velocity, size)
            )),
            None
        )
        case Pause => se.state.pause()
        case Restart => se.state.restart()
        case Toggle => if (se.state.isRunning) se.state.pause() else se.state.restart()
    }

    def flatMap(g: Point => PaintDrawing) = DefaultInteractionStateTransition(
        point => f(point).flatMap(g)
    )
}

