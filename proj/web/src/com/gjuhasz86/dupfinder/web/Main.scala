package com.gjuhasz86.dupfinder.web

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}
import scala.scalajs.LinkingInfo
import slinky.core._
import slinky.web.ReactDOM
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.raw.XMLHttpRequest
import slinky.core._
import slinky.core.annotations.react
import slinky.readwrite.Writer
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.SyntheticMouseEvent
import sun.security.tools.PathList

import scala.scalajs.js.annotation.JSExport
//import io.circe.generic.auto._
import slinky.core.facade.Fragment

//@JSExportTopLevel("Main")
object Main {

  //  @JSExport
  @JSExportTopLevel("main")
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

