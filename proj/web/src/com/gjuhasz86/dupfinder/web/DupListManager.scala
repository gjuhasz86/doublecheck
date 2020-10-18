package com.gjuhasz86.dupfinder.web

import io.circe.generic.extras.Configuration
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class DupListPath(path: String, active: Boolean)
case class DupListState(roots: List[Node], paths: List[String]) {
  private val rootSet = roots.map(_.path).toSet
  val richPaths = paths.map { p => DupListPath(p, rootSet.contains(p)) }
}
@react class DupListManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults
  type State = DupListState
  override def render(): ReactElement = props.children(state)

  case class Props(children: DupListState => ReactElement)
  override def initialState: State = DupListState(Nil, Nil)


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
      setState(_.copy(paths = dups.sorted))
    }

}
