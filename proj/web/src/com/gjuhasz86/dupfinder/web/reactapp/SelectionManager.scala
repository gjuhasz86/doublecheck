package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement


case class SelectionMgrState(lastSelected: Int, selected: Set[NodeLite], dragOp: SelectionManagerModels.SelCmd.Op)
@react class SelectionManager extends Component {
  import SelectionManagerModels.SelCmd._
  import SelectionManagerModels._

  case class Props(items: List[NodeLite], children: SelectionMgrState => ReactElement)
  type State = SelectionMgrState
  override def initialState = SelectionMgrState(0, Set(), SelCmd.Op.Add)
  override def render(): ReactElement = props.children(state)

  def makeSelection(cmd: SelCmd[NodeLite]) = (cmd match {
    case cmd@All(_) => cmd.normalize(props.items, state.lastSelected)
    case cmd@ByIdx(_, _) => cmd.normalize(props.items)
    case cmd@ByItem(_, _) => cmd.normalize(props.items)
    case cmd@ByRangeCont(_, _) => cmd.normalize(props.items, state.lastSelected)
    case cmd@SelEvent(_, _, _) => cmd
  }) match {
    case SelEvent(Op.CleanAdd, items, idx) => setState(_.copy(lastSelected = idx, selected = items.toSet))
    case SelEvent(Op.Add, items, idx) => setState(_.copy(lastSelected = idx, selected = state.selected ++ items.inner))
    case SelEvent(Op.Rem, items, idx) => setState(_.copy(lastSelected = idx, selected = state.selected -- items.inner))
    case SelEvent(Op.Toggle, items, idx) =>
      val newSelected = items.foldLeft(state.selected)((acc, e) => if (acc.contains(e)) acc - e else acc + e)
      setState(_.copy(lastSelected = idx, selected = newSelected))
  }

  def clear() = setState(_.copy(lastSelected = 0, selected = Set()))
  def cleanAdd(node: NodeLite) = makeSelection(ByItem(Op.CleanAdd, node))
  def selectAll() = makeSelection(All(Op.Add))
  def add(node: NodeLite) = makeSelection(ByItem(Op.Add, node))
  def rem(node: NodeLite) = makeSelection(ByItem(Op.Rem, node))
  def toggle(node: NodeLite) = makeSelection(ByItem(Op.Toggle, node))
  def addRange(idx: Int) = makeSelection(ByRangeCont(Op.Add, idx))

  def dragFrom(node: NodeLite) = if (state.selected.contains(node)) setDrag(Op.Rem) else setDrag(Op.Add)
  def setDrag(op: Op) = setState(_.copy(dragOp = op))
  def dragOn(node: NodeLite) = makeSelection(ByItem(state.dragOp, node))
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

    case class All[+A](op: Op) extends SelCmd[A] {
      def normalize[B >: A](items: List[B], lastIdx: Int): SelEvent[B] = SelEvent.of(op, items, lastIdx)
    }
    case class ByItem[+A](op: Op, a: A) extends SelCmd[A] {
      def normalize[B >: A](items: List[B]): SelEvent[B] = SelEvent.of(op, List(a), items.indexOf(a))
    }
    case class ByIdx[+A](op: Op, idx: Int) extends SelCmd[A] {
      def normalize[B >: A](items: List[B]): SelEvent[B] = SelEvent.of(op, List(items(idx)), idx)
    }
    case class ByRangeCont(op: Op, idx: Int) extends SelCmd[Nothing] {
      def normalize[A](items: List[A], lastIdx: Int): SelEvent[A] = {
        val its = (idx until lastIdx by (lastIdx - idx).sign).map(items.apply).toList
        SelEvent.of(op, its, idx)
      }
    }
    case class SelEvent[+A](op: SelCmd.Op, items: UniqList[A], idx: Int) extends SelCmd[A]
    object SelEvent {
      import UniqList._

      def of[A](op: Op, items: List[A], idx: Int): SelEvent[A] = new SelEvent(op, items.uniq, idx)
    }
  }

}