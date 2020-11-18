package com.gjuhasz86.dupfinder.web.reactapp

import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.SyntheticMouseEvent
import slinky.web.html._

@react object TextButton {

  case class Props(active: Boolean, clickHandler: SyntheticMouseEvent[TagElement#RefType] => Unit, children: ReactElement)
  val component = FunctionalComponent[Props] { props =>
    div(
      className := (if (props.active) "textBtn active" else "textBtn"),
      onClick := props.clickHandler)(props.children)
  }

}
