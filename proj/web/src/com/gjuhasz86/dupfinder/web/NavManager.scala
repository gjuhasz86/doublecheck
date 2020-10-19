package com.gjuhasz86.dupfinder.web

import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class NavState(roots: List[Node], nodes: List[Node])
@react class NavManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults
  type State = NavState
  override def render(): ReactElement = props.children(state)

  case class Props(children: NavState => ReactElement)
  override def initialState: State = NavState(Nil, Nil)

  def setRoots(roots: List[Node]) =
    setState(_.copy(roots = roots))

  def reset() = setState(initialState)

  val req = NodeReq(Set(), ChildSelection.Direct, filters = Set())

  override def componentDidMount(): Unit = {
    fetchNodes(req.copy(roots = state.roots.map(_.path).toSet))
  }

  override def componentDidUpdate(prevProps: Props, prevState: State): Unit = {
    if (prevState.roots != state.roots) {
      fetchNodes(req.copy(roots = state.roots.map(_.path).toSet))
    }
  }

  def fetchNodes(req: NodeReq): Unit =
    FetchUtils.postBackend("search", req.asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[Node]](res)
      setState(_.copy(nodes = nodes))
    }

}
