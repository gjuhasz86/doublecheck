package com.gjuhasz86.dupfinder.web.laminarapp

case class FlatNode(
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
  children: List[FlatNode] = Nil
)

object FlatNode {
  val Empty: FlatNode = FlatNode("", "", "", 0, "", Map(), 0, 0, 0, 0, 0, Nil)
}
