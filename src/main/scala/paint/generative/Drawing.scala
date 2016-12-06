package paint.generative

import org.scalajs.dom.CanvasRenderingContext2D
import paint.geometry.Geometry.DoublePoint
import paint.algebra.{PaintAlgebra, PaintDrawing, Point}
import paint.events.CanvasEvent

/**
  * Created by nic on 04/12/2016.
  */
trait Drawing[E] {
    def event(e: E): Drawing[E]
    def newPoint(position: DoublePoint): E
    def draw(): CanvasRenderingContext2D => Unit
}

case class TransitionDrawing[E](
    currentState: State,
    transition: StateTransition[E],
    newPointToEvent: DoublePoint => E,
    drawOnCanvasAlgebra: PaintAlgebra[CanvasRenderingContext2D => Unit]
) extends Drawing[E] {
    override def event(e: E): Drawing[E] = {
        this.copy(currentState = transition.next(StateWithEvent(currentState, e)))
    }
    override def draw(): (CanvasRenderingContext2D) => Unit = {
        currentState.drawing(drawOnCanvasAlgebra)
    }
    override def newPoint(position: DoublePoint): E = newPointToEvent(position)
}
