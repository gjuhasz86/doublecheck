package com.gjuhasz86.dupfinder.web.simplified

import org.scalajs.dom.raw.HTMLInputElement
import slinky.core._
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._

import scala.scalajs.js
import scala.util.Try

object MainApp extends ComponentWrapper {

  type Props = Unit
  case class State(panelCount: Int, links: Map[Int, Int], currLink: Option[Int]) {
    def addLink(from: Int, to: Int) = copy(links = links + (to -> from))
    def startLink(idx: Int) = copy(currLink = Some(idx))
    def stopLink = copy(currLink = None)
  }
  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = State(4, Map(), None)

    def handleIdClick(idx: Int) = state.currLink match {
      case Some(from) => setState(state.addLink(from, idx).stopLink)
      case None => setState(state.copy(currLink = Some(idx)))
    }

    def handleResultChange(panels: Seq[ReactRef[Panel.Def]], SourceIdx: Int, result: Int) =
      state.links.toList
        .collect { case (to, SourceIdx) => to }
        .foreach(idx => panels(idx).current.setNum1(result))

    def render(): ReactElement = {
      val panels = (0 until state.panelCount).map(_ => React.createRef[Panel.Def])

      div(
        state.currLink.map { l =>
          div(onClick := (_ => setState(state.stopLink)))(s"Currently linking [$l] ... click here to cancel")
        },
        div(state.links.toList.map { case (k, v) => span(s"$k -> $v, ") }),
        (0 until state.panelCount).toList.map { idx =>
          div(
            button(onClick := (_ => handleIdClick(idx)))(idx, state.links.get(idx).map(x => s" <- $x")),
            Panel(Panel.Props(onResultChange = handleResultChange(panels, idx, _))).withRef(panels(idx)))
        },
        button(onClick := (_ => setState(state.copy(panelCount = state.panelCount + 1))))("[ADD PANEL]")
      )
    }
  }
}

object Panel extends ComponentWrapper {
  val fn: Map[String, (Int, Int) => Int] =
    Map("ADD" -> (_ + _), "MUL" -> (_ * _), "POW" -> (Math.pow(_, _).toInt))

  case class Props(onResultChange: Int => Unit)
  case class State(num1: Int, num2: Int, op: String) {
    def result = Panel.fn(op)(num1, num2)
  }

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    def initialState: State = State(0, 0, "ADD")

    val nextOp = Map("ADD" -> "MUL", "MUL" -> "POW", "POW" -> "ADD")

    def setNum1(n: Int) = setState(state.copy(num1 = n))
    def setNum2(n: Int) = setState(state.copy(num2 = n))
    def setOp(op: String) = setState(state.copy(op = op))

    override def componentDidUpdate(p: Props, prev: State): Unit =
      if (prev.result != state.result) {
        props.onResultChange(state.result)
      }

    def render(): ReactElement = {
      div(
        input(onChange := (e => setNum1(Try(e.target.asInstanceOf[HTMLInputElement].value.toInt).getOrElse(0)))),
        span(onClick := (_ => setOp(nextOp(state.op))))(state.op),
        input(onChange := (e => setNum2(Try(e.target.asInstanceOf[HTMLInputElement].value.toInt).getOrElse(0)))),
        span(s" = ${state.result}")
      )
    }
  }
}