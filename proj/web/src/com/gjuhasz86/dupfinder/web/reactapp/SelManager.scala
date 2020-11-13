package com.gjuhasz86.dupfinder.web.reactapp

import slinky.core.facade.Hooks._

object SelManager {
  import SelectionManagerModels.SelCmd._
  import SelectionManagerModels._

  def useSelection[T](items: List[T]) = {
    val (lastSelected, setLastSelected) = useState(0)
    val (selected, setSelected) = useState(UniqList[T]())
    val (dragOp, setDragOp) = useState[SelectionManagerModels.SelCmd.Op](SelCmd.Op.Add)

    new SelMgr[T] {

      override def selection: UniqList[T] = selected

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
          setSelected(selected ++ items)
        case SelEvent(Op.Rem, items, idx) =>
          setLastSelected(idx)
          setSelected(selected -- items)
        case SelEvent(Op.Toggle, items, idx) =>
          val newSelected = items.foldLeft(selected)((acc, e: T) => if (acc.contains(e)) acc - e else acc + e)
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

      override def dragFrom(node: T) = if (selected.contains(node)) setDrag(Op.Rem) else setDrag(Op.Add)
      override def setDrag(op: Op) = setDragOp(op)
      override def dragOn(node: T) = makeSelection(ByItem(dragOp, node))

    }

  }

  trait SelMgr[T] {
    def selection: UniqList[T]
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
}