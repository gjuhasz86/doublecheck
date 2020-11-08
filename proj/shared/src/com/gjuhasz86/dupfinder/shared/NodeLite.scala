package com.gjuhasz86.dupfinder.shared
import com.gjuhasz86.dupfinder.shared.Stat._

case class NodeLite(path: String, stats: Stats) {
  def nodetype = stats(classOf[NType]).map(_.value).getOrElse(NodeType.Oth)
  def ntype = stats(classOf[NType]).map(_.value.short).getOrElse("")
  def hash = stats(classOf[Hash]).map(_.value)
  def size = stats(classOf[Size]).map(_.value).getOrElse(0)
  def name = stats(classOf[Name]).map(_.value).getOrElse("")
  def hashes = stats(classOf[Hashes]).map(_.value)
  def childCount = stats(classOf[ChildCount]).map(_.value)
  def childFileCount = stats(classOf[ChildFileCount]).map(_.value)
  def dummyCount = stats(classOf[DummyCount]).map(_.value)
  def dupCount = stats(classOf[DupCount]).map(_.value)
  def leafDupCount = stats(classOf[LeafDupCount]).map(_.value)
  def extDupCount = stats(classOf[ExtDupCount]).map(_.value)
  def selfDupCount = stats(classOf[SelfDupCount]).map(_.value)
}

object NodeLite {
  val Empty = NodeLite("", Stats.empty)
}

sealed trait Stat
object Stat {
  case class NType(value: NodeType) extends Stat
  case class Hash(value: String) extends Stat
  case class Size(value: Long) extends Stat
  case class Name(value: String) extends Stat
  case class Hashes(value: Set[String]) extends Stat
  case class ChildCount(value: Int) extends Stat
  case class DummyCount(value: Int) extends Stat
  case class DupCount(value: Int) extends Stat
  case class LeafDupCount(value: Int) extends Stat
  case class ExtDupCount(value: Int) extends Stat
  case class SelfDupCount(value: Int) extends Stat
  case class ChildFileCount(value: Int) extends Stat
}

case class Stats(map: Map[String, Stat]) {
  def updated(s: Stat) = copy(map.updated(s.getClass.getName, s))
  def apply[A <: Stat](c: Class[A]): Option[A] = map.get(c.getName).asInstanceOf[Option[A]]
}

object Stats {
  def empty: Stats = Stats(Map.empty)
}
