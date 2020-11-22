package com.gjuhasz86.dupfinder.web.reactapp

import slinky.core.facade.Hooks._
import SelectionManagerModels.SelCmd._
import SelectionManagerModels._

trait SelMgr[T] {
  def selected: UniqList[T]
  def makeSelection(cmd: SelectionManagerModels.SelCmd[T]): Unit
  def clear(): Unit
  def cleanAdd(node: T): Unit
  def selectAll(): Unit
  def add(node: T): Unit
  def rem(node: T): Unit
  def toggle(node: T): Unit
  def addRange(idx: Int): Unit
  def dragFrom(node: T): Unit
  def setDrag(op: SelCmd.Op): Unit
  def dragOn(node: T): Unit
}

object SelMgr {
  def useSelection[T](items: List[T]) = {
    val (lastSelected, setLastSelected) = useState(0)
    val (selectedState, setSelected) = useState(UniqList[T]())
    val (dragOp, setDragOp) = useState[SelectionManagerModels.SelCmd.Op](SelCmd.Op.Add)

    new SelMgr[T] {

      override def selected: UniqList[T] = selectedState

      override def makeSelection(cmd: SelCmd[T]) = (cmd match {
        case cmd@All(_) => cmd.normalize(items, lastSelected)
        case cmd@ByIdx(_, _) => cmd.normalize(items)
        case cmd@ByItem(_, _) => cmd.normalize(items)
        case cmd@ByRangeCont(_, _) => cmd.normalize(items, lastSelected)
        case cmd@SelEvent(_, _, _) => cmd
      }) match {
        case SelEvent(Op.CleanAdd, items, idx) =>
          setLastSelected(idx)
          setSelected(items)
        case SelEvent(Op.Add, items, idx) =>
          setLastSelected(idx)
          setSelected(selectedState ++ items)
        case SelEvent(Op.Rem, items, idx) =>
          setLastSelected(idx)
          setSelected(selectedState -- items)
        case SelEvent(Op.Toggle, items, idx) =>
          val newSelected = items.foldLeft(selectedState)((acc, e: T) => if (acc.contains(e)) acc - e else acc + e)
          setLastSelected(idx)
          setSelected(newSelected)
      }

      override def clear() = {
        setLastSelected(0)
        setSelected(UniqList[T]())
      }
      override def cleanAdd(node: T) = makeSelection(ByItem(Op.CleanAdd, node))
      override def selectAll() = makeSelection(All(Op.Add))
      override def add(node: T) = makeSelection(ByItem(Op.Add, node))
      override def rem(node: T) = makeSelection(ByItem(Op.Rem, node))
      override def toggle(node: T) = makeSelection(ByItem(Op.Toggle, node))
      override def addRange(idx: Int) = makeSelection(ByRangeCont(Op.Add, idx))

      override def dragFrom(node: T) = if (selectedState.contains(node)) setDrag(Op.Rem) else setDrag(Op.Add)
      override def setDrag(op: Op) = setDragOp(op)
      override def dragOn(node: T) = makeSelection(ByItem(dragOp, node))

    }

  }

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