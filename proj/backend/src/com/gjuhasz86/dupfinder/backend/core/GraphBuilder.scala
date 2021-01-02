package com.gjuhasz86.dupfinder.backend.core

import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDateTime

import better.files._
import com.gjuhasz86.dupfinder.backend.core.Utils._
import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.NodeType
import com.gjuhasz86.dupfinder.shared.Stat
import com.gjuhasz86.dupfinder.shared.Stat.Hashes
import com.gjuhasz86.dupfinder.shared.Stat.LeafDupCount
import com.gjuhasz86.dupfinder.shared.Stats
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.MarkType
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.shared.request.NodeSelection

import scala.annotation.tailrec
import scala.collection.mutable

class GraphBuilder(nodesFile: File, hashFile: File, excludeFile: File, markFile: File) {

  val excluded = excludeFile.lines.filterNot(_.trim.startsWith("`")).toSet

  def isExcludedPath(path: String) =
    excluded.exists(ignorePath => Paths.get(path).startsWith(ignorePath))
  def isExcludedHash(hash: String) =
    excluded.contains(s"#$hash")
  def isExcluded(node: Node) = isExcludedPath(node.path) || isExcludedHash(node.hash)
  def isExcluded(path: String, hash: String) = isExcludedPath(path) || isExcludedHash(hash)

  val (ignored, deleted, saved) =
    markFile.lines.map(_.split(",", 2)).map {
      case Array("Ignored", f) => (Some(f), None, None)
      case Array("Deleted", f) => (None, Some(f), None)
      case Array("Saved", f) => (None, None, Some(f))
    }.unzip3 match {
      case (i, d, s) => (i.flatten.to(mutable.Set), d.flatten.to(mutable.Set), s.flatten.to(mutable.Set))
    }

  def persistMarks() = {
    val marks =
      ignored.map { f => s"Ignored,$f" } ++
        deleted.map { f => s"Deleted,$f" } ++
        saved.map { f => s"Saved,$f" }

    markFile.overwrite(marks.mkString("\n"))
    marks.size
  }

  def mark(markType: Option[MarkType], nodes: List[NodeLite]) = {
    if (markType.contains(MarkType.Ignored)) {
      ignored.addAll(nodes.map(_.path))
    } else {
      ignored.subtractAll(nodes.map(_.path))
    }

    if (markType.contains(MarkType.Deleted)) {
      deleted.addAll(nodes.map(_.path))
    } else {
      deleted.subtractAll(nodes.map(_.path))
    }

    if (markType.contains(MarkType.Saved)) {
      saved.addAll(nodes.map(_.path))
    } else {
      saved.subtractAll(nodes.map(_.path))
    }

    nodes.map(node => node.copy(stats = node.stats.updated(Stat.Marked(markType))))
  }

  def getMark(node: Node) =
    if (ignored.contains(node.path))
      Some(MarkType.Ignored)
    else if (deleted.contains(node.path))
      Some(MarkType.Deleted)
    else if (saved.contains(node.path))
      Some(MarkType.Saved)
    else
      None

  val hashesByPath: Map[String, String] = {
    var counter = 0
    hashFile.lineIterator.map(_.split(" {2}", 2))
      .foldLeft(Map[String, String]()) {
        case (acc, Array(hash, path)) =>
          counter += 1
          if (counter % 50000 == 0) {println(counter)}
          if (isExcluded(path, hash))
            acc
          else
            acc + (path -> hash)
      }

  }

  val pathsByHash: Map[String, List[String]] =
    hashesByPath.groupBy(_._2).map { case (k, v) => k -> v.keys.toList }

  def activePathsByHash(hash: String): List[String] =
    pathsByHash.getOrElse(hash, Nil)
      .filterNot(p => ignored.contains(p) || deleted.contains(p))

  val root: Node = {

    def nodes0: LazyList[Node] =
      nodesFile
        .lineIterator
        .map(_.split(",", 3))
        .map { case Array(NodeType(ntype), size, path) =>
          Node(ntype, path, size.toLong, hashesByPath.getOrElse(path, DummyHash))(Nil, pathsByHash)
        }
        .filterNot(node => isExcluded(node))
        .to(LazyList)

    var counter = 0
    /**
     * Builds a tree of nodes.
     * Algorithm:
     * (1) if no node has been started to process yet
     * ** (2) take the first unprocessed node and add it as the first incomplete parent with no children
     * (3) if the next unprocessed node is a children of the latest parent node
     * ** (4) take it and add it as a children of the latest parent node, then add it as the latest parent node with no children
     * (5) if the next unprocessed node is not a children of the latest parent node
     * ** (6) complete the latest parent node by setting its children from the children list and
     * ** (7) taking it from the parents list and append as a child to its parent's children list
     * (8) if there are no unprocessed nodes and there is only a single parent
     * ** (9) set the children of the parent from the children list and return it
     *
     * @param nodes    remaining unprocessed nodes
     * @param children children of nodes in parents list in order
     * @param parents  list of incomplete nodes
     * @return the root node
     */
    @tailrec def loop(nodes: LazyList[Node], children: List[List[Node]], parents: List[Node]): Node =
      (nodes, children, parents) match {
        case (node #:: restNodes, Nil, Nil) => // (1)
          loop(restNodes, List(Nil), List(node)) // (2)
        case (node #:: restNodes, currChilds :: restChilds, parent :: restNs) if Paths.get(node.path).startsWith(parent.path) => // (3)
          loop(restNodes, List() :: (node :: currChilds) :: restChilds, node :: parent :: restNs) // (4)
        case (nodes, currChilds :: (_ :: prevChilds) :: restChilds, parent :: restNs) => // (5)
          counter += 1
          val updatedParent = parent.copy()(currChilds, pathsByHash) // (6)
          if (counter % 10000 == 0) {println(s"$counter | ${parents.size}")}
          loop(nodes, (updatedParent :: prevChilds) :: restChilds, restNs) // (7)
        case (LazyList(), rootChilds :: Nil, root :: Nil) => // (8)
          val updatedRoot = root.copy()(rootChilds, pathsByHash) // (9)
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

  val recChildHashes: Map[String, List[String]] = {
    @tailrec def loop(unvisited: List[Node], acc: Map[String, List[String]]): Map[String, List[String]] =
      unvisited match {
        case Nil => acc
        case head :: tail =>
          val newAcc = acc ++ allParents(head.path).map(p => p -> (head.hash :: acc.getOrElse(p, Nil)))
          loop(head.children ::: tail, newAcc)
      }

    loop(List(root), Map())
  }

  def dupCount(node: Node) =
    recChildHashes(node.path)
      .filterNot(_ == DummyHash)
      .map(activePathsByHash)
      .filter(_.size > 1)
      .flatMap {_.filter(Paths.get(_).startsWith(node.path))}
      .distinct
      .size

  def extDupCount(node: Node) =
    recChildHashes(node.path)
      .distinct
      .map { hash =>
        val (internal, external) = activePathsByHash(hash).partition(Paths.get(_).startsWith(node.path))
        if (external.isEmpty) 0 else internal.size
      }.sum

  def dummyCount(node: Node) =
    recChildHashes(node.path).count(_ == DummyHash)

  def emptyFileCount(node: Node) = {
    val children = allChildren(List(node))
    children.toList.count(x => x.ntype == NodeType.Fil(true) && x.size == 0)
  }

  def childDirCount(node: Node) = {
    val children = allChildren(List(node))
    children.toList.map(_.ntype).count { case NodeType.Dir(_) => true; case _ => false }
  }

  def childFileCount(node: Node) = {
    val children = allChildren(List(node))
    children.toList.map(_.ntype).count { case NodeType.Fil(_) => true; case _ => false }
  }

  def selfDupCount(node: Node): Int =
    activePathsByHash(node.hash).toSet.excl(node.path).size

  def safeDupCount(node: NodeLite): Int =
    node.hash.map(safeDupCount(_, node.path)).getOrElse(0)

  def safeDupCount(node: Node): Int =
    safeDupCount(node.hash, node.path)

  def safeDupCount(hash: String, path: String): Int =
    pathsByHash.getOrElse(hash, Nil).count(p => saved.contains(p) && p != path)

  def search(req: NodeReq) = {

    val rootNodes = req.roots.flatMap(nodesByPath.get)
    val selected = req.selection match {
      case NodeSelection.DirectChildren => rootNodes.flatMap(_.children).map(enrichNode)
      case NodeSelection.DeepChildren => allChildren(rootNodes.toList).map(enrichNode)
      case NodeSelection.DupNodes => dupsStr(req.roots.toList)
    }

    selected.filter { node =>
      req.filters.forall {
        case ChildFilter.DescendantOf(paths) =>
          paths.exists(n => Paths.get(node.path).startsWith(n))
        case ChildFilter.NonEmpty =>
          node.nodetype match {
            case NodeType.Fil(_) => node.size != 0
            case _ => true
          }
        case ChildFilter.Empty =>
          node.nodetype match {
            case NodeType.Fil(_) => node.size == 0
            case _ => true
          }
        case ChildFilter.NodeTypeIn(ntypes) =>
          ntypes.contains(node.ntype)
        case ChildFilter.HasDups =>
          !ignored.contains(node.path) &&
            !deleted.contains(node.path) &&
            activePathsByHash(node.hash.get).exists(p => p != node.path)
        case ChildFilter.HasExtDups =>
          !ignored.contains(node.path) &&
            !deleted.contains(node.path) &&
            activePathsByHash(node.hash.get)
              .filterNot(p => p == node.path)
              .exists(path => req.roots.exists(root => !Paths.get(path).startsWith(root)))
        case ChildFilter.Ignored => ignored.contains(node.path)
        case ChildFilter.Deleted => deleted.contains(node.path)
        case ChildFilter.Inactive => ignored.contains(node.path) || deleted.contains(node.path)
        case ChildFilter.Active => !ignored.contains(node.path) && !deleted.contains(node.path)
        case ChildFilter.NoSafeDup => safeDupCount(node) == 0
      }
    }

  }

  def allChildren(nodes: List[Node]): Set[Node] = {
    @tailrec
    def loop(acc: Set[Node], unvisited: List[Node]): Set[Node] = unvisited match {
      case Nil => acc
      case head :: rest if acc.contains(head) => loop(acc, rest)
      case head :: rest if !acc.contains(head) => loop(acc + head, head.children ::: rest)
    }
    loop(Set(), nodes.flatMap(_.children))
  }

  def dupsStr(roots: List[String]) = {
    val rootSet = roots.map(Paths.get(_)).toSet

    val leafSet =
      roots
        .map(x => x -> hashesByPath.get(x))
        .flatMap {
          case (path, None) =>
            println(s"Couldn't find hash for path [$path]")
            None
          case (_, hash) => hash
        }
        .distinct
        .flatMap(pathsByHash.get)
        .flatten
        .map(Paths.get(_))
        .toSet

    val leafHashes: Set[String] = (leafSet -- rootSet).map(n => nodesByPath(n.toString).hash)

    leafSet
      .flatMap(allParentNodes)
      .map { node =>
        val leafDupHashes = recChildHashes(node.path).toSet.intersect(leafHashes)
        val lnode = enrichNode(node)
        lnode.copy(stats =
          lnode.stats
            .updated(Hashes(leafDupHashes))
            .updated(LeafDupCount(leafDupHashes.size))
        )
      }
  }

  def allParentNodes(segments: Path): List[Node] = {
    (1 to segments.getNameCount).map(segments.subpath(0, _).toString).map(nodesByPath).toList
  }

  def allParents(path: String): Seq[String] =
    allParents(Paths.get(path))

  def allParents(segments: Path): Seq[String] =
    (1 to segments.getNameCount).map(segments.subpath(0, _).toString)


  def enrichNode(node: Node) = {
    import com.gjuhasz86.dupfinder.shared.Stat._
    NodeLite(node.path,
      Stats.empty
        .updated(NType(node.ntype))
        .updated(Name(node.name))
        .updated(Hash(node.hash))
        .updated(Size(node.size))
        .updated(DupCount(dupCount(node)))
        .updated(ExtDupCount(extDupCount(node)))
        .updated(DummyCount(dummyCount(node)))
        .updated(EmptyFileCount(emptyFileCount(node)))
        .updated(ChildCount(recChildHashes(node.path).size - 1))
        .updated(ChildDirCount(childDirCount(node)))
        .updated(ChildFileCount(childFileCount(node)))
        .updated(SelfDupCount(selfDupCount(node)))
        .updated(SafeDupCount(safeDupCount(node)))
        .updated(Marked(getMark(node))))
  }
}