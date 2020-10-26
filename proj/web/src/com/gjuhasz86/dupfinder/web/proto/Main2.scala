package com.gjuhasz86.dupfinder.web.proto

import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveElement
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.html

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.annotation.JSExportTopLevel

object Main2 {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults


  @JSExportTopLevel("main")
  def main(): Unit = {

    val navMgr = new NavManager
    val chdMgr = new ChildrenManager
    val rootMgr = new RootManager

    val rootElement = div(
      navMgr,
      chdMgr,
      navMgr.currentNode --> chdMgr.root.writer,
      rootMgr.root --> navMgr.rootBus,
      div(
        cls := "breadcrumbHolder",
        children <-- navMgr.parentsStack.map(_.zipWithIndex.reverse).splitIntoSignals(_._1.path) { (_, _, nodeIdx) =>
          val node = nodeIdx.map(_._1)
          val idx = nodeIdx.map(_._2)
          div(
            cls := "breadcrumb", cls := "textBtn",
            inContext(_.events(onClick).sample(idx).map(NavCmd.Up.apply) --> navMgr.navBus.writer),
            node.asText(_.name))
        }.map(x => x.intersperse(div(cls := "breadcrumb", ">")))
      ),
      table(
        thead(
          tr(td("typ"), td("name"), td("cld"), td("dmy"), td("nzf"), td("dup"), td("ext"), td("sdc"))
        ),
        tbody(
          tr(
            hidden <-- navMgr.parentsStack.map(_.size <= 1),
            onClick.mapTo(NavCmd.Up()) --> navMgr.navBus.writer,
            cls := "nodeRow",
            td("D"), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
          ),
          children <-- chdMgr.children.splitIntoSignals(_.path) { (_, _, node) =>
            tr(
              cls := "nodeRow",
              inContext(_.events(onClick).sample(node).map(NavCmd.Down.apply) --> navMgr.navBus.writer),
              td(node.asText(_.ntype)),
              td(node.asText(_.name)),
              td(node.asText(_.childCount)),
              td(node.asText(_.dummyCount)),
              td(node.asText(_.stats.getOrElse("F", 0))),
              td(node.asText(_.dupCount)),
              td(node.asText(_.extDupCount)),
              td(node.asText(_.selfDupCount)),
            )
          }
        )
      )
    )

    val containerNode = dom.document.querySelector("#root")
    render(containerNode, rootElement)
  }

  implicit class EventStreamOps[A](val self: EventStream[A]) extends AnyVal {
    def asText[B](f: A => B) =
      child.text <-- self.map(f(_).toString)
  }

  implicit class SignalOps[A](val self: Signal[A]) extends AnyVal {
    def asText[B](f: A => B) =
      child.text <-- self.map(f(_).toString)
  }

  implicit class ListOps[A](val self: List[A]) extends AnyVal {
    def intersperse(sep: => A): List[A] = {
      self match {
        case Nil => Nil
        case _ => self.flatMap(el => List(sep, el)).tail
      }
    }
  }

  sealed trait NavCmd
  object NavCmd {
    case class Up(n: Int = 1) extends NavCmd
    case class Down(node: Node) extends NavCmd
    case class Root(node: Node) extends NavCmd
  }


  class NavManager extends Component[html.Div] {
    val navBus: EventBus[NavCmd] = new EventBus[NavCmd]
    val rootBus: EventBus[Node] = new EventBus[Node]

    val parentsStack: Signal[List[Node]] = {
      navBus.events.foldLeft(List[Node]())((parents, nav) => nav match {
        case NavCmd.Root(node) => List(node)
        case NavCmd.Up(n) => parents.drop(n min (parents.size - 1))
        case NavCmd.Down(node) => node :: parents
      })
    }

    val currentNode: Signal[Node] = parentsStack.map(_.headOption.getOrElse(Node.Empty))

    override val rel: ReactiveElement[html.Div] =
      div(
        hidden := true,
        rootBus.events.map(NavCmd.Root) --> navBus.writer
      )
  }

  class ChildrenManager extends Component[html.Div] {
    override val rel = div(hidden := true)

    val root: EventBus[Node] = new EventBus[Node]

    val children: Signal[List[Node]] =
      root.events
        .flatMap(node => EventStream.fromFuture(fetchChildren(node)))
        .map(_.sortBy(n => (n.ntype, n.name)))
        .toSignal(Nil)

    private def fetchChildren(node: Node): Future[List[Node]] =
      FetchUtils.postPath("children", node.path.asJson.noSpaces).map { res =>
        decode[List[Node]](res).getOrElse(null)
      }
  }

  class RootManager extends Component[html.Div] {
    override val rel = div(hidden := true)
    val root = EventStream.fromFuture(fetchRoot)

    private def fetchRoot: Future[Node] =
      FetchUtils.getPath("root").map { res =>
        decode[Node](res).getOrElse(null)
      }
  }

  class SelectionManager[A] extends Component[html.Div] {
    override val rel = div(hidden := true)

    val items: Var[List[A]] = Var(List[A]())
    val selectedItems: Signal[Set[A]] = ???

    val lastSelectedIdx: Signal[Set[A]] = ???

    private val toggleSelectionBus = new EventBus[A]
    val toggleSelection: WriteBus[A] = toggleSelectionBus.writer


  }

  sealed trait SelectionCmd[+A]
  object SelectionCmd {
    case class Add[A](a: A) extends SelectionCmd[A]
    case class Rem[A](a: A) extends SelectionCmd[A]
    case class Toggle[A](a: A) extends SelectionCmd[A]
    case class AddIdx(idx: Int) extends SelectionCmd[Nothing]
    case class RemIdx(idx: Int) extends SelectionCmd[Nothing]
    case class ToggleIdx(idx: Int) extends SelectionCmd[Nothing]
  }
}
