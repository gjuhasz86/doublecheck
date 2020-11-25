package com.gjuhasz86.dupfinder.web.simplified.nomacro

import org.scalajs.dom.raw.HTMLInputElement
import slinky.core._
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._
import slinky.web.ReactDOM

import scala.scalajs.js
import scala.util.Try
import scala.scalajs.js.annotation._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Hooks._
import slinky.web.html._
import rx._
import slinky.core.facade.React

object MainApp {

  type Props = Unit
  val component = FunctionalComponent[Props] { _ =>

    val (panelCount, setPanelCount) = useState(1)
    val (links, setLinks) = useState(Map[Int, Int]())
    val (currLink, setCurrLink) = useState(None: Option[Int])

    val outLinks = links.toList.groupBy(_._2).map { case (from, tos) => from -> tos.map(_._1) }


    // println(s"creating vars in main")
    val numInputs = Var(-1 -> 0)
    val nums = Var(-1 -> 0)
    val obs = nums.trigger((n: (Int, Int)) => println(s"Received $n"))
    useEffect(() => () => obs.kill())

    nums.map { case (srcIdx, n) =>
      outLinks.getOrElse(srcIdx, Nil).foreach(to => numInputs.update(to -> n))
    }

    div(
      div(onClick := (_ => setCurrLink(None)))("[",
        currLink.map(lnk => s"Currently linking [$lnk] ... click here to cancel"), "]"
      ),
      div("[", links.toList.map { case (to, from) => span(s"$from -> $to, ") }, "]"),
      (1 to panelCount).toList.map { idx =>
        div(
          button(onClick := (_ => currLink match {
            case Some(lnk) if lnk != idx => setLinks(links + (idx -> lnk)); setCurrLink(None)
            case Some(lnk) if lnk == idx => setLinks(links -- Set(idx)); setCurrLink(None)
            case None => setCurrLink(Some(idx))
          }))(idx, links.get(idx).map(x => s" <- $x")),
          Panel.component(Panel.Props(numInputs.filter(_._1 == idx).map(_._2), r => nums.update(idx -> r)))
        )
      },
      button(onClick := (_ => setPanelCount(panelCount + 1)))("[ADD PANEL]")
    )
  }
}

object Panel {
  case class Props(inputStream: Rx[Int], onResultChange: Int => Unit)
  val component = FunctionalComponent[Props] { props =>
    // println(s"creating panel")
    val (op, setOp) = useState("ADD")
    val (num1, setNum1) = useState(0)
    val (num2, setNum2) = useState(0)

    val n1Ref = React.createRef[org.scalajs.dom.html.Input]
    val n2Ref = React.createRef[org.scalajs.dom.html.Input]

    val fn: (Int, Int) => Int = op match {
      case "ADD" => _ + _
      case "MUL" => _ * _
      case "POW" => Math.pow(_, _).toInt
    }

    val result = fn(num1, num2)
    useEffect(() => {
      // println(s"result updated [$result]")
      props.onResultChange(result)
    }, List(result))


    val obs = props.inputStream.triggerLater { (n: Int) =>
      // println(s"Setting num1 to [$n]")
      setNum1(n)
      Option(n1Ref.current).foreach(_.value = n.toString)
    }
    useEffect(() => () => obs.kill())

    val nextOp = Map("ADD" -> "MUL", "MUL" -> "POW", "POW" -> "ADD")

    def handleChange1() = {
      val n = Try(n1Ref.current.value.toInt).getOrElse(0)
      // println(s"Changed 1 : [$n]")
      setNum1(n)
    }
    def handleChange2() = {
      val n = Try(n2Ref.current.value.toInt).getOrElse(0)
      // println(s"Changed 2: [$n]")
      setNum2(n)
    }

    div(
      input(ref := n1Ref, onChange := (_ => handleChange1())),
      span(onClick := (_ => setOp(nextOp(op))))(op),
      input(ref := n2Ref, onChange := (_ => handleChange2())),
      span(s" = $result")
    )
  }
}