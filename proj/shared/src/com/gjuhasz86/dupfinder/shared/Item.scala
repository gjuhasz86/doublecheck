package com.gjuhasz86.dupfinder.shared

case class Item(name: String, stats: List[Stat])
sealed trait Stat
object Stat {
  case class NodeType(ntype: String) extends Stat
  case class Hash(hash: String) extends Stat
  case class Size(size: Int) extends Stat
  case class Hashes(hashes: Set[String]) extends Stat
}
