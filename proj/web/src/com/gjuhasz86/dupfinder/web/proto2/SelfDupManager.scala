package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

import scala.annotation.tailrec

case class AggrNode(path: String, hashes: Set[String])
case class SelfDupMgrState(loading: Boolean, rootSet: Set[String], nodes: List[Node]) {

  private def allParents(path: String) = {
    val segments = path.split("/").toList
    segments.indices.map(i => segments.take(i).mkString("/")).toList
  }

  val aggr = {
    val preRes: List[AggrNode] =
      nodes
        .filterNot(n => rootSet.contains(n.path))
        .flatMap(n => allParents(n.path).map(p => p -> n.hash))
        .groupBy { case (p, _) => p }
        .map { case (p, hs) => AggrNode(p, hs.map(_._2).toSet) }
        .toList
        .sortBy(_.path)

    @tailrec
    def norm(list: List[AggrNode], acc: List[AggrNode]): List[AggrNode] =
      list match {
        case a :: b :: rest if a.hashes == b.hashes => norm(b :: rest, acc)
        case a :: rest => norm(rest, a :: acc)
        case Nil => acc
      }

    norm(preRes, Nil).reverse
  }
}

@react class SelfDupManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  case class Props(children: SelfDupMgrState => ReactElement)
  type State = SelfDupMgrState
  override def initialState = SelfDupMgrState(loading = false, Set(), Nil)
  override def render(): ReactElement = props.children(state)

  def loadChildren(roots: List[Node]) =
    fetchNodes(roots)

  private def fetchNodes(roots: List[Node]): Unit = {
    setState(_.copy(loading = true))
    FetchUtils.postBackend("dups", roots.map(_.hash).asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[Node]](res)
      setState(_.copy(loading = false, rootSet = roots.map(_.path).toSet, nodes = nodes.sortBy(_.path)))
    }
  }
}
