package com.gjuhasz86.dupfinder.backend.core

import better.files._
import com.gjuhasz86.dupfinder.backend.core.Utils.DummyHash
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq

import scala.annotation.tailrec

class GraphBuilder(nodesFile: File, hashFile: File) {

  val hashesByPath: Map[String, String] = {
    var counter = 0
    hashFile.lineIterator.map(_.split(" {2}", 2)).foldLeft(Map[String, String]()) {
      case (acc, Array(hash, name)) =>
        counter += 1
        if (counter % 50000 == 0) {println(counter)}
        acc + (name -> hash)
    }
  }

  val pathsByHash: Map[String, List[String]] =
    hashesByPath.groupBy(_._2).map { case (k, v) => k -> v.keys.toList }


  val root: Node = {

    def nodes0: LazyList[Node] =
      nodesFile
        .lineIterator
        .map(_.split(",", 3))
        .map { case Array(NodeType(ntype), size, path) =>
          Node(ntype, path, size.toLong, hashesByPath.getOrElse(path, DummyHash))(Nil, pathsByHash)
        }
        .to(LazyList)

    var counter = 0
    @tailrec
    def loop(nodes: LazyList[Node], children: List[List[Node]], parents: List[Node]): Node =
      (nodes, children, parents) match {
        case (node #:: restNodes, Nil, Nil) =>
          loop(restNodes, List(Nil), List(node))
        case (node #:: restNodes, currChilds :: restChilds, parent :: restNs) if node.path.startsWith(parent.path) =>
          loop(restNodes, List() :: (node :: currChilds) :: restChilds, node :: parent :: restNs)
        case (nodes, currChilds :: (_ :: prevChilds) :: restChilds, parent :: restNs) =>
          counter += 1
          val updatedParent = parent.copy()(currChilds, pathsByHash)
          if (counter % 10000 == 0) {println(s"$counter | ${parents.size}")}
          loop(nodes, (updatedParent :: prevChilds) :: restChilds, restNs)
        case (LazyList(), rootChilds :: Nil, root :: Nil) =>
          val updatedRoot = root.copy()(rootChilds, pathsByHash)
          updatedRoot
      }

    loop(nodes0, Nil, Nil)
  }

  val nodesByPath: Map[String, Node] = {
    @tailrec
    def loop(acc: Map[String, Node], nodes: List[Node]): Map[String, Node] =
      nodes match {
        case Nil => acc
        case node :: rest =>
          loop(acc + (node.path -> node), node.children ::: rest)
      }
    loop(Map(), root :: Nil)
  }

  def search(req: NodeReq) = {

    val rootNodes = req.roots.flatMap(nodesByPath.get)
    val selected = req.selection match {
      case ChildSelection.All => rootNodes.flatMap(_.children)
      case ChildSelection.Direct => allChildren(rootNodes.toList)
    }

    selected.filter { node =>
      req.filters.forall {
        case ChildFilter.NonEmpty =>
          node.size != 0
        case ChildFilter.NodeTypeIn(ntypes) =>
          ntypes.contains(node.ntype.short)
        case ChildFilter.HasDups =>
          pathsByHash.getOrElse(node.hash, Nil).size > 1
        case ChildFilter.HasExtDups =>
          pathsByHash.getOrElse(node.hash, Nil).exists(path => req.roots.exists(root => path.startsWith(root)))
      }
    }

  }

  private def allChildren(nodes: List[Node]): Set[Node] = {
    def loop(acc: Set[Node], unvisited: List[Node]): Set[Node] = unvisited match {
      case Nil => acc
      case head :: rest if acc.contains(head) => loop(acc, rest)
      case head :: rest if !acc.contains(head) => loop(acc + head, head.children ::: rest)
    }
    loop(Set(), nodes)
  }
}