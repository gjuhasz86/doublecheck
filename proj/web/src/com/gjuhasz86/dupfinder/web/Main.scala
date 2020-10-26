package com.gjuhasz86.dupfinder.web

import org.scalajs.dom
import slinky.web.ReactDOM

import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  //  @JSExportTopLevel("main")
  def main(): Unit = {
    //import scala.scalajs.LinkingInfo
    //import slinky.core._
    //    if (LinkingInfo.developmentMode) {
    //      hot.initialize()
    //    }

    val container = Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }

    ReactDOM.render(App(), container)
  }
}
