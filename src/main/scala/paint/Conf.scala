package paint

import org.scalajs.dom
import org.scalajs.dom._
import paint.html.NativeRenderingContext

/**
  * Created by nic on 26/11/2016.
  */
object Conf {
    lazy val canvasInitializer: CanvasInitializer =
        FullWindowCanvasInitializer(dom.document, dom.window) andThen
        ColorCanvasInitializer("black")
}
