package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._


case class SelectionMgrState(lastSelected: Int, selected: Set[Node], dragOp: SelectionManagerModels.SelCmd.Op)
@react class SelectionManager extends Component {
  import SelectionManagerModels._
  import SelectionManagerModels.SelCmd._

  case class Props(items: List[Node], children: SelectionMgrState => ReactElement)
  type State = SelectionMgrState
  override def initialState = SelectionMgrState(0, Set(), SelCmd.Op.Add)
  override def render(): ReactElement = props.children(state)

  def makeSelection(cmd: SelCmd[Node]) = (cmd match {
    case cmd@ByIdx(_, _) => cmd.normalize(props.items)
    case cmd@ByItem(_, _) => cmd.normalize(props.items)
    case cmd@ByRangeCont(_, _) => cmd.normalize(props.items, state.lastSelected)
    case cmd@SelEvent(_, _, _) => cmd
  }) match {
    case SelEvent(Op.CleanAdd, items, idx) => setState(_.copy(lastSelected = idx, selected = items))
    case SelEvent(Op.Add, items, idx) => setState(_.copy(lastSelected = idx, selected = state.selected ++ items))
    case SelEvent(Op.Rem, items, idx) => setState(_.copy(lastSelected = idx, selected = state.selected -- items))
    case SelEvent(Op.Toggle, items, idx) =>
      val newSelected = items.foldLeft(state.selected)((acc, e) => if (acc.contains(e)) acc - e else acc + e)
      setState(_.copy(lastSelected = idx, selected = newSelected))
  }

  def clear() = setState(_.copy(lastSelected = 0, selected = Set()))
  def cleanAdd(node: Node) = makeSelection(ByItem(Op.CleanAdd, node))
  def add(node: Node) = makeSelection(ByItem(Op.Add, node))
  def rem(node: Node) = makeSelection(ByItem(Op.Rem, node))
  def toggle(node: Node) = makeSelection(ByItem(Op.Toggle, node))
  def addRange(idx: Int) = makeSelection(ByRangeCont(Op.Add, idx))

  def dragFrom(node: Node) = if (state.selected.contains(node)) setDrag(Op.Rem) else setDrag(Op.Add)
  def setDrag(op: Op) = setState(_.copy(dragOp = op))
  def dragOn(node: Node) = makeSelection(ByItem(state.dragOp, node))
}

object SelectionManagerModels {
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