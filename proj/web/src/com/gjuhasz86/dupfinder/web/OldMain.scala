package com.gjuhasz86.dupfinder.web

import org.scalajs.dom
import slinky.web.ReactDOM
//import io.circe.generic.auto._

//@JSExportTopLevel("Main")
object OldMain {

  //  @JSExport
  //  @JSExportTopLevel("main")
  def main(): Unit = {
    val container = Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }

    ReactDOM.render(HelloName(name = "sdfsd"), container)
  }
}

