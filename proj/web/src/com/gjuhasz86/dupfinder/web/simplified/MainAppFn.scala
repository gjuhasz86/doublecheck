package com.gjuhasz86.dupfinder.web.simplified

import org.scalajs.dom.raw.HTMLInputElement
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Hooks._
import slinky.web.html._

@react object MainAppFn {

  type Props = Unit
  val component = FunctionalComponent[Props] { _ =>

    val (panelCount, setPanelCount) = useState(1)

    div(
      (1 to panelCount).toList.map { i =>
        PanelFn()
      },
      div(
        className := "textBtn addPanel",
        onClick := (_ => setPanelCount(panelCount + 1))
      )("[ADD PANEL]")
    )
  }
}

@react object PanelFn {

  type Props = Unit
  val component = FunctionalComponent[Props] { _ =>
    val (op, setOp) = useState("ADD")
    val (num1, setNum1) = useState(0)
    val (num2, setNum2) = useState(0)

    val fn: (Int, Int) => Int = op match {
      case "ADD" => _ + _
      case "MUL" => _ * _
      case "POW" => Math.pow(_, _).toInt
    }

    val nextOp = Map("ADD" -> "MUL", "MUL" -> "POW", "POW" -> "ADD")

    div(
      input(onChange := (e => setNum1(e.target.asInstanceOf[HTMLInputElement].value.toIntOption.getOrElse(0)))),
      span(onClick := (_ => setOp(nextOp(op))))(op),
      input(onChange := (e => setNum2(e.target.asInstanceOf[HTMLInputElement].value.toIntOption.getOrElse(0)))),
      span(s" = ${fn(num1, num2)}")
    )
  }
}