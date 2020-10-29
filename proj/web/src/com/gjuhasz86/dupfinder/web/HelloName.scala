package com.gjuhasz86.dupfinder.web

import com.gjuhasz86.dupfinder.shared.Node

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}
import scala.scalajs.LinkingInfo
import slinky.core._
import slinky.web.ReactDOM
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.document
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
//import io.circe.generic.auto._
import slinky.core.facade.Fragment

@react class HelloName extends StatelessComponent {
  case class Props(name: String)

  val node = Node(props.name, 1337)

  override def componentDidMount() = {
    document.addEventListener("contextmenu)", ((e: Any) => println("CC")))
  }
  def render() = {
    div(
      h1(
        onClick := (_ => println("mmm")),
        onKeyPress := (_ => println("kkkkkkkk")),
        onContextMenu := { e => e.preventDefault(); println("ccccccc") },
        s"Hello Slinky [$node] !!"),
      div("a"),
      div("sssssssssssssssssssssssssssssssssssssssssssssssssss")
    )
  }
}