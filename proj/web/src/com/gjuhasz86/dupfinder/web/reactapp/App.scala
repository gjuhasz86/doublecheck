package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import rx._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Hooks._
import slinky.core.facade.Fragment
import slinky.web.html._

@react object App {

  type Props = Unit
  val component = FunctionalComponent[Props] { _ =>

    val (panels, setPanels) = useState((1 to 1).toList)
    val (links, setLinks) = useState(Map[Int, Int]())
    val (currLink, setCurrLink) = useState(None: Option[Int])
    val (minimized, setMinimized) = useState(Set[Int]())
    val (wide, setWide) = useState(Set[Int]())
    val (color, setColor) = useState(false)
    val (flex, setFlex) = useState(false)

    val nodeInputs = Var(-1 -> List[NodeLite]())
    //    val obs = nodeInputs.trigger(n => println(s"Received $n"))
    //    useEffect(() => () => obs.kill())
    div(
      div(
        div(
          className := "textBtn" + (if (color) " active" else ""),
          onClick := (_ => setColor(!_))
        )("[COLOR]"),
        div(
          className := "textBtn" + (if (flex) " active" else ""),
          onClick := (_ => setFlex(!_))
        )("[FLEX]")
      ),
      div(className := "panelArea" + (if (color) " alternate" else "") + (if (flex) " flex" else " cols"))(
        panels.map { id =>
          Fragment(
            div(className := Seq("panelOuter rowBreak", if (wide.contains(id)) "wide" else "").mkString(" ")),
            div(
              key := id.toString,
              className := Seq(
                "panelOuter",
                if (wide.contains(id)) "wide" else "").mkString(" ")
            )(
              div(className := "panelHead")(
                div(
                  className := "textBtn",
                  onClick := (_ => currLink match {
                    case None => setCurrLink(Some(id))
                    case Some(lnk) if lnk == id => setCurrLink(None)
                    case Some(lnk) if lnk < id => setLinks(links + (id -> lnk)); setCurrLink(None)
                    case Some(_) =>
                  })
                )(currLink match {
                  case None => "[LINK]"
                  case Some(lnk) if lnk == id => "[CANCEL]"
                  case Some(lnk) if lnk < id => s"[LINK TO #$lnk]"
                  case Some(_) => ""
                }),
                div(className := "panelTitle")(s"PANEL #$id", links.get(id).map(src => s" <- #$src")),
                div(
                  className := (if (links.keySet.contains(id)) "textBtn" else "textBtn hidden"),
                  onClick := (_ => setLinks(links -- Set(id)))
                )("[UNLINK]"),
                div(
                  className := "textBtn panelFirstRightButton",
                  onClick := (_ => if (wide.contains(id)) setWide(_ - id) else setWide(_ + id))
                )(if (wide.contains(id)) "[ >-< ]" else "[ <-> ]"),
                div(
                  className := "textBtn",
                  onClick := (_ => if (minimized.contains(id)) setMinimized(_ - id) else setMinimized(_ + id))
                )(if (minimized.contains(id)) "[ = ]" else "[ _ ]"),
                div(
                  className := "textBtn",
                  onClick := { _ =>
                    setPanels(panels.filter(_ != id))
                    setLinks(links.filterNot { case (k, v) => k == id || v == id })
                  }
                )("[ X ]")
              ),
              hr(),
              Panel(
                id,
                className = Seq(
                  "panel",
                  if (minimized.contains(id)) "minimized" else "",
                ).mkString(" "),
                nodes => nodeInputs.update(id -> nodes),
                nodeInputs.filter { case (src, _) => links.get(id).contains(src) }.map(_._2)
              )
            ),
            div(className := Seq("panelOuter rowBreak", if (wide.contains(id)) "wide" else "").mkString(" ")),
          )
        },
        div(
          className := "textBtn addPanel panelOuter",
          onClick := (_ => setPanels(panels :+ (panels.maxOption.getOrElse(0) + 1)))
        )("[ADD PANEL]")
      )
    )
  }
}
