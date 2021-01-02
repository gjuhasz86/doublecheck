package com.gjuhasz86.dupfinder.shared
import com.gjuhasz86.dupfinder.shared.Stat._
import com.gjuhasz86.dupfinder.shared.request.MarkType

case class NodeLite(path: String, stats: Stats) {
  def nodetype = stats(NType).map(_.value).getOrElse(NodeType.Oth)
  def ntype = stats(NType).map(_.value.short).getOrElse("")
  def hash = stats(Hash).map(_.value)
  def size = stats(Size).map(_.value).getOrElse(0)
  def name = stats(Name).map(_.value).getOrElse("")
  def hashes = stats(Hashes).map(_.value)
  def childCount = stats(ChildCount).map(_.value)
  def childFileCount = stats(ChildFileCount).map(_.value)
  def dummyCount = stats(DummyCount).map(_.value)
  def dupCount = stats(DupCount).map(_.value)
  def leafDupCount = stats(LeafDupCount).map(_.value)
  def extDupCount = stats(ExtDupCount).map(_.value)
  def selfDupCount = stats(SelfDupCount).map(_.value)
  def safeDupCount = stats(SafeDupCount).map(_.value)
  def mark = stats(Marked).flatMap(_.value)
}

object NodeLite {
  val Empty = NodeLite("", Stats.empty)
}

sealed abstract class Stat(val key: StatKey[_])
sealed trait StatKey[+T] {def name: String = this.toString}
object StatKey {
  val values: List[StatKey[_]] = {
    List(NType, Hash, Size, Name, Hashes, ChildCount, DummyCount, EmptyFileCount,
      DupCount, LeafDupCount, ExtDupCount, SelfDupCount, SafeDupCount, ChildFileCount, ChildDirCount, Marked)
  }
  def of(key: String): Option[StatKey[_]] = values.find(_.name == key)
}

object Stat {
  case object NType extends StatKey[NType]
  case class NType(value: NodeType) extends Stat(NType)
  case object Hash extends StatKey[Hash]
  case class Hash(value: String) extends Stat(Hash)
  case object Size extends StatKey[Size]
  case class Size(value: Long) extends Stat(Size)
  case object Name extends StatKey[Name]
  case class Name(value: String) extends Stat(Name)
  case object Hashes extends StatKey[Hashes]
  case class Hashes(value: Set[String]) extends Stat(Hashes)
  case object ChildCount extends StatKey[ChildCount]
  case class ChildCount(value: Int) extends Stat(ChildCount)
  case object DummyCount extends StatKey[DummyCount]
  case class DummyCount(value: Int) extends Stat(DummyCount)
  case object EmptyFileCount extends StatKey[EmptyFileCount]
  case class EmptyFileCount(value: Int) extends Stat(EmptyFileCount)
  case object DupCount extends StatKey[DupCount]
  case class DupCount(value: Int) extends Stat(DupCount)
  case object LeafDupCount extends StatKey[LeafDupCount]
  case class LeafDupCount(value: Int) extends Stat(LeafDupCount)
  case object ExtDupCount extends StatKey[ExtDupCount]
  case class ExtDupCount(value: Int) extends Stat(ExtDupCount)
  case object SelfDupCount extends StatKey[SelfDupCount]
  case class SelfDupCount(value: Int) extends Stat(SelfDupCount)
  case object SafeDupCount extends StatKey[SafeDupCount]
  case class SafeDupCount(value: Int) extends Stat(SafeDupCount)
  case object ChildFileCount extends StatKey[ChildFileCount]
  case class ChildFileCount(value: Int) extends Stat(ChildFileCount)
  case object ChildDirCount extends StatKey[ChildDirCount]
  case class ChildDirCount(value: Int) extends Stat(ChildDirCount)
  case object Marked extends StatKey[Marked]
  case class Marked(value: Option[MarkType]) extends Stat(Marked)
}

case class Stats(map: Map[StatKey[_], Stat]) {
  def updated(s: Stat) = copy(map.updated(s.key, s))
  def apply[A <: Stat](key: StatKey[A]): Option[A] = map.get(key).asInstanceOf[Option[A]]
}

object Stats {
  def empty: Stats = Stats(Map.empty)
}
