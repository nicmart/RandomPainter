package paint

import org.scalajs.dom
import org.scalajs.dom._

/**
  * Created by nic on 26/11/2016.
  */
object Conf {
    lazy val canvasInitializer =
        FullWindowCanvasInitializer(dom.document, dom.window) andThen
        ColorCanvasInitializer("black")

    def canvas(canvasRenderingContext2D: CanvasRenderingContext2D) =
        PrimitiveCanvas(canvasRenderingContext2D)
}
