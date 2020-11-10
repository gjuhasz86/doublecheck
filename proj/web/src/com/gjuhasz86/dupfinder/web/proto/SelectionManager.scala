package com.gjuhasz86.dupfinder.web.proto


import com.gjuhasz86.dupfinder.web.FetchUtils
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


class SelectionManager[A] extends Component[html.Div] {
  import SelectionManager._
  import SelectionManager.SelCmd._


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

object SelectionManager {
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