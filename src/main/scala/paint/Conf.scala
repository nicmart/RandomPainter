package paint

import org.scalajs.dom

/**
  * Created by nic on 26/11/2016.
  */
object Conf {
    lazy val canvasInitializer: CanvasInitializer =
        FullWindowCanvasInitializer(dom.document, dom.window) andThen
        ColorCanvasInitializer("black")
}
