package com.gjuhasz86.dupfinder.backend.core

import better.files._
import com.gjuhasz86.dupfinder.backend.core.Utils._
import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.Stats

case class Node(ntype: NodeType, path: String, size: Long, hash: String)(val children: List[Node], private val pathsByHash: Map[String, List[String]]) {
  val name = path.toFile.name
  val stats: Map[NodeType, Int] = (Map(ntype -> 1) :: children.map(_.stats)).merged
  val hashes: List[String] = hash :: children.flatMap(_.hashes)
  val dupCount = dups.size
  val extDupCount = externalDups.size
  val dummyCount = hashes.count(_ == DummyHash)
  val childCount = hashes.size

  val selfDupCount = pathsByHash.get(hash).map(_.size).getOrElse(0)

  def dups = hashes.filterNot(_ == DummyHash).map(pathsByHash).filter(_.size > 1)
  def externalDups = hashes.filterNot(_ == DummyHash).map(pathsByHash).filter(paths => paths.exists(!_.startsWith(path)))

  def externalDupPaths = externalDups.flatten.filter(_.startsWith(path))

}
