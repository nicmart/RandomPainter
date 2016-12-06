package paint.html

import org.scalajs.dom.CanvasRenderingContext2D
import paint.geometry.Geometry.DoublePoint

/**
  * Wrapper on native js rendering context
  */
trait RenderingContext {
    def fillRect(x: Double, y: Double, w: Double, h: Double): Unit
}

class NativeRenderingContext(inner: CanvasRenderingContext2D) extends RenderingContext {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): Unit = {
        inner.fillRect(x, y, w, h)
    }
}

class TransformedCanvasRenderingContext2D(
    original: RenderingContext,
    transformation: DoublePoint => DoublePoint
) extends RenderingContext {
    override def fillRect(x: Double, y: Double, w: Double, h: Double): Unit = {
        val from = DoublePoint(x, y)
        val to = from + DoublePoint(w, h)
        val newFrom = transformation(from)
        val newTo = transformation(to)
        original.fillRect(
            newFrom.x,
            newFrom.y,
            w,
            h
        )
    }
}
