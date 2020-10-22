package com.gjuhasz86.dupfinder.web

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class NavState(
  roots: List[Node], childSelection: ChildSelection, filter: Set[ChildFilter],
  dirty: Boolean, autoFetch: Boolean, nodes: List[Node])
@react class NavManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults
  type State = NavState
  override def render(): ReactElement = props.children(state)

  case class Props(children: NavState => ReactElement)
  override def initialState: State = NavState(Nil, ChildSelection.Direct, Set(), false, false, Nil)

  def setRoots(roots: List[Node]) =
    setState(_.copy(roots = roots, dirty = true))

  def setChildSelection(cs: ChildSelection) =
    setState(_.copy(childSelection = cs, dirty = cs != state.childSelection))

  def addFilter(cf: ChildFilter) =
    setState(_.copy(filter = state.filter + cf, dirty = !state.filter.contains(cf)))

  def delFilter(cf: ChildFilter) =
    setState(_.copy(filter = state.filter - cf, dirty = state.filter.contains(cf)))

  def toggleFilter(cf: ChildFilter) =
    if (state.filter.contains(cf)) delFilter(cf) else addFilter(cf)

  def clearFilter() =
    setState(_.copy(filter = Set(), dirty = state.filter.nonEmpty))

  def toggleAutoFetch() =
    setState(_.copy(autoFetch = !state.autoFetch))

  def reset() = setState(initialState)

  def req = NodeReq(state.roots.map(_.path).toSet, state.childSelection, state.filter)

  def forceFetch() =
    fetchNodes(req)

  override def componentDidMount(): Unit =
    forceFetch()

  override def componentDidUpdate(prevProps: Props, prevState: State): Unit = {
    if (state.dirty && state.autoFetch) {
      setState(_.copy(dirty = false))
      forceFetch()
    }
  }

  def fetchNodes(req: NodeReq): Unit =
    FetchUtils.postBackend("search", req.asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[Node]](res)
      setState(_.copy(nodes = nodes))
    }

}
