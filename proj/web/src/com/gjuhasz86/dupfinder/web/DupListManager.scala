package com.gjuhasz86.dupfinder.web

import io.circe.generic.extras.Configuration
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class DupListPath(path: String, active: Boolean)
case class DupListState(roots: List[Node], rawPaths: List[String], aggr: Boolean) {
  private val rootSet = roots.map(_.path).toSet
  val aggrPaths = {
    val preRes =
      rawPaths
        .filterNot(rootSet.contains)
        .foldLeft(List[String]()) { (acc, p) =>
          val path = p.split("/").toList
          val paths = (1 to path.size).map(i => path.take(i).mkString("/")).toList
          paths ::: acc
        }
        .groupBy(identity)
        .map { case (k, v) => v.size -> k }
        .filter(_._1 > 1)
        .toList

    preRes
      .filter { case (count, path) => !preRes.exists { case (c, p) => p != path && p.startsWith(path) && c >= count } }
      .sortBy(x => (x._1 * -1, x._2))
      .map { case (c, p) => s"[$c] $p" }
  }
  val paths = if (aggr) aggrPaths else rawPaths

  val richPaths = paths.map { p => DupListPath(p, rootSet.contains(p)) }

}
@react class DupListManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults
  type State = DupListState
  override def render(): ReactElement = props.children(state)

  case class Props(children: DupListState => ReactElement)
  override def initialState: State = DupListState(Nil, Nil, false)

  def setAggr(aggr: Boolean) =
    setState(_.copy(aggr = aggr))

  def setRoots(roots: List[Node]) =
    setState(_.copy(roots = roots))

  def reset() = setState(initialState)

  override def componentWillUpdate(nextProps: Props, nextState: DupListState): Unit = {
    if (nextState.roots != state.roots) {
      fetchDups(nextState.roots.map(_.hash))
    }
  }

  def fetchDups(hashes: List[String]): Unit =
    FetchUtils.postBackend("dups", hashes.asJson.noSpaces) { res =>
      val Right(dups) = decode[List[String]](res)
      setState(_.copy(rawPaths = dups.sorted))
    }

}
