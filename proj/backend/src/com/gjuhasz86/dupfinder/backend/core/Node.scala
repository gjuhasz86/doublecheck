package com.gjuhasz86.dupfinder.backend.core

import better.files._
import com.gjuhasz86.dupfinder.backend.core.Utils._
import com.gjuhasz86.dupfinder.shared.NodeType

case class Node(ntype: NodeType, path: String, size: Long, hash: String)(val children: List[Node], private val pathsByHash: Map[String, List[String]]) {
  val name = path.toFile.name
  def stats: Map[NodeType, Int] = (Map(ntype -> 1) :: children.map(_.stats)).merged
//  def hashes: List[String] = hash :: children.flatMap(_.hashes)
//  def dupCount: Int = dups.size
//  def extDupCount: Int = externalDups.size
//  def dummyCount: Int = hashes.count(_ == DummyHash)
//  def childCount: Int = hashes.size
//
//  def selfDupCount: Int = pathsByHash.get(hash).map(_.size).getOrElse(0)
//
//  def dups = hashes.filterNot(_ == DummyHash).map(pathsByHash).filter(_.size > 1)
//  def externalDups = hashes.filterNot(_ == DummyHash).map(pathsByHash).filter(paths => paths.exists(!_.startsWith(path)))
//
//  def externalDupPaths = externalDups.flatten.filter(_.startsWith(path))


//  private def toLite = {
//    import com.gjuhasz86.dupfinder.shared.Stat._
//    NodeLite(path,
//      Stats.empty
//        .updated(NType(ntype))
//        .updated(Name(name))
//        .updated(Hash(hash))
//        .updated(Size(size))
//        .updated(DupCount(dupCount))
//        .updated(ExtDupCount(extDupCount))
//        .updated(DummyCount(dummyCount))
//        .updated(ChildCount(childCount))
//        .updated(SelfDupCount(selfDupCount))
//        .updated(ChildFileCount(stats.getOrElse(NodeType.Fil(true), 0)))
//    )
//  }
}
