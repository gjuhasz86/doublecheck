package com.gjuhasz86.dupfinder.web.reactapp
import slinky.core.facade.Hooks._

import scala.annotation.tailrec

trait ColMgr[A] {
  def colHeaders: List[ColMgr.ColHeader]
  def columns: List[ColMgr.ColDef[A]]

  def rem(name: String): Unit
  def add(name: String): Unit
  def toggle(name: String): Unit
  def visible(name: String): Boolean
  def move(name: String, idx: Int): Unit
}

object ColMgr {

  def useColumns[A](colMap: Map[String, ColDef[A]])(defaultCols: List[ColHeader]): ColMgr[A] = {
    val (columnsState, setColumns) = useState(defaultCols)

    new ColMgr[A] {
      def colHeaders = columnsState

      def columns: List[ColDef[A]] =
        columnsState.collect { case ColHeader(name, true) => colMap(name) }

      def rem(name: String) =
        if (columnsState.count(_.visible) > 1) {
          setColumns(columnsState.map(c => if (c.name == name) c.copy(visible = false) else c))
        }

      def add(name: String) =
        setColumns(columnsState.map(c => if (c.name == name) c.copy(visible = true) else c))

      def toggle(name: String) =
        if (visible(name)) rem(name) else add(name)

      def visible(name: String): Boolean =
        columnsState.find(_.name == name).exists(_.visible)

      def move(name: String, idx: Int): Unit = {
        val selected = columnsState.find(_.name == name).get
        @tailrec
        def loop(acc: List[ColHeader], remaining: List[ColHeader]): List[ColHeader] =
          remaining match {
            case Nil => acc :+ selected
            case curr :: rest if (curr :: acc).count(_.visible) > idx =>
              acc ::: selected :: curr :: rest
            case curr :: rest =>
              loop(acc :+ curr, rest)
          }

        setColumns(loop(Nil, columnsState.filterNot(_.name == name)))
      }

    }
  }

  trait ColDef[A] {
    def name: String
    def get(node: A): Option[String]
  }
  case class ColHeader(name: String, visible: Boolean)
}
