package com.gjuhasz86.dupfinder.web

import org.scalajs.dom
import org.scalajs.dom.raw.Element
import slinky.web.ReactDOM

import scala.scalajs.js.annotation.JSExportTopLevel


object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {
    //import scala.scalajs.LinkingInfo
    //import slinky.core._
    //    if (LinkingInfo.developmentMode) {
    //      hot.initialize()
    //    }

    val container: Element = Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }

    ReactDOM.render(simplified.MainAppFn(), container)
    //    ReactDOM.render(proto2.DebugComp(), container)
  }
}
