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
    val selMgr = new SelectionManager[Node]

    val debugBus = new EventBus[Any]
    debugBus.events.foreach(x => println(s"DEBUG: $x"))(new Owner {})

    val rootElement = div(
      navMgr,
      chdMgr,
      rootMgr,
      selMgr,
      navMgr.currentNode --> chdMgr.root.writer,
      rootMgr.root --> navMgr.rootBus,
      chdMgr.children --> selMgr.items.writer,
      chdMgr.root.events.mapTo(SelCmd.SelEvent(SelCmd.Op.CleanAdd, Set(), 0)) --> selMgr.command,
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
      div(selMgr.selectedItems.map(_.size).signal.asText(_.toString)),
      table(
        //        inContext(_.events(onMouseEnter).map(_.buttons == 1) --> selMgr.dragSelectVar.writer),
        //        inContext(_.events(onMouseLeave).map(_.buttons == 1) --> selMgr.dragSelectVar.writer),
        thead(
          tr(td("typ"), td("name"), td("cld"), td("dmy"), td("nzf"), td("dup"), td("ext"), td("sdc"))
        ),
        tbody(
          tr(
            hidden <-- navMgr.parentsStack.map(_.size <= 1),
            onDblClick.mapTo(NavCmd.Up()) --> navMgr.navBus.writer,
            cls := "nodeRow",
            td("D"), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
          ),
          children <-- chdMgr.children.map(_.zipWithIndex.map { case (node, idx) =>
            tr(
              cls := "nodeRow",
              cls.toggle("selectedRow") <-- selMgr.selectedItems.map(_.contains(node)),
              inContext(_.events(onDblClick).mapToValue(node).map(NavCmd.Down.apply) --> navMgr.navBus.writer),
              inContext(_.events(onClick).collect {
                case e if e.ctrlKey => SelCmd.ByItem(SelCmd.Op.Toggle, node)
                case e if e.shiftKey => SelCmd.ByRangeCont(SelCmd.Op.Add, idx)
                case _ => SelCmd.ByItem(SelCmd.Op.CleanAdd, node)
              } --> selMgr.command),
              //              inContext(_.events(onContextMenu, preventDefault = true).mapToValue(s"CTX [$node]") --> debugBus.writer),
              inContext(_.events(onMouseDown).sample(selMgr.selectedItems).map(x => if (x.contains(node)) SelCmd.Op.Rem else SelCmd.Op.Add) --> selMgr.dragSelectVar.writer),
              inContext(_.events(onMouseEnter).filter(_.buttons == 1).sample(selMgr.dragSelectVar.signal).map(op => SelCmd.ByItem(op, node)) --> selMgr.command),
              inContext(_.events(onMouseLeave).filter(_.buttons == 1).sample(selMgr.dragSelectVar.signal).map(op => SelCmd.ByItem(op, node)) --> selMgr.command),
              td(node.ntype),
              td(node.name),
              td(node.childCount),
              td(node.dummyCount),
              td(node.stats.getOrElse[Int]("F", 0)),
              td(node.dupCount),
              td(node.extDupCount),
              td(node.selfDupCount),
            )
          })
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
    import SelCmd._


    val items: Var[List[A]] = Var(List[A]())

    private val commandBus = new EventBus[SelCmd[A]]
    val command: WriteBus[SelCmd[A]] = commandBus.writer

    private val lastSelIdxVar: Var[Int] = Var(0)
    val lastSelectedIdx: Signal[Int] = lastSelIdxVar.signal

    private val selEvents: EventStream[SelEvent[A]] =
      commandBus.events
        .withCurrentValueOf(items.signal)
        .withCurrentValueOf(lastSelectedIdx.signal)
        .map {
          case ((cmd@ByIdx(_, _), items), _) => cmd.normalize(items)
          case ((cmd@ByItem(_, _), items), _) => cmd.normalize(items)
          case ((cmd@ByRangeCont(_, _), items), lastIdx) => cmd.normalize(items, lastIdx)
          case ((cmd@SelEvent(_, _, _), _), _) => cmd
        }
    //        .debugSpy { e =>
    //          val ee = e.copy(items = e.asInstanceOf[SelEvent[Node]].items.map(_.name))
    //          println(s"Received selEvent [$ee]")
    //        }

    val selectedItems: Signal[Set[A]] =
      selEvents.foldLeft(Set[A]())((acc, e) => e match {
        case SelEvent(Op.Add, items, _) => acc ++ items
        case SelEvent(Op.CleanAdd, items, _) => items
        case SelEvent(Op.Rem, items, _) => acc -- items
        case SelEvent(Op.Toggle, items, _) =>
          items.foldLeft(acc)((acc2, el) => if (acc2.contains(el)) acc - el else acc + el)
      })

    val dragSelectVar: Var[Op] = Var(Op.Add)

    override val rel = div(
      hidden := true,
      selEvents.map(_.idx) --> lastSelIdxVar.writer
    )
  }
  sealed trait SelCmd[+A]
  object SelCmd {
    sealed trait Op
    object Op {
      case object Add extends Op
      case object CleanAdd extends Op
      case object Rem extends Op
      case object Toggle extends Op
    }

    case class ByItem[A](op: Op, a: A) extends SelCmd[A] {
      def normalize(items: List[A]) = SelEvent(op, Set(a), items.indexOf(a))
    }
    case class ByIdx[A](op: Op, idx: Int) extends SelCmd[A] {
      def normalize(items: List[A]) = SelEvent(op, Set(items(idx)), idx)
    }
    case class ByRangeCont(op: Op, idx: Int) extends SelCmd[Nothing] {
      def normalize[A](items: List[A], lastIdx: Int) = {
        val its = (idx until lastIdx by (lastIdx - idx).sign).map(items.apply).toSet
        SelEvent(op, its, idx)
      }
    }
    case class SelEvent[A](op: SelCmd.Op, items: Set[A], idx: Int) extends SelCmd[A]
  }
}
