package com.gjuhasz86.dupfinder.web

case class Node(
  ntype: String,
  path: String,
  name: String,
  size: Long,
  hash: String,
  stats: Map[String, Int],
  dupCount: Int,
  extDupCount: Int,
  dummyCount: Int,
  childCount: Int,
  selfDupCount: Int,
  children: List[Node] = Nil
)

object Node {
  val Empty: Node = Node("", "", "", 0, "", Map(), 0, 0, 0, 0, 0, Nil)
}
