package com.gjuhasz86.dupfinder.web.reactapp

import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Hooks._
import slinky.web.html._


@react object App {

  type Props = Unit
  val component = FunctionalComponent[Props] { _ =>

    val (panelCount, setPanelCount) = useState(1)

    div(
      (1 to panelCount).toList.map { i =>
        Panel()
      },
      div(
        className := "textBtn addPanel",
        onClick := (_ => setPanelCount(panelCount + 1))
      )("[ADD PANEL]")
    )
  }
}
