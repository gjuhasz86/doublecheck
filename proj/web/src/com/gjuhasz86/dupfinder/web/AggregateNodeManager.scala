package com.gjuhasz86.dupfinder.web

import io.circe.generic.extras.Configuration
import io.circe.parser._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import io.circe.generic.extras.auto._
import io.circe.syntax._

case class AggregateNodeState(path: String, nodes: List[Node])

@react class AggregateNodeManager extends Component {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  case class Props(children: AggregateNodeState => ReactElement)
  type State = AggregateNodeState

  override def initialState: State = AggregateNodeState("", Nil)

  override def render(): ReactElement =
    props.children(state)

  private def fetchAllExt(path: String): Unit =
    FetchUtils.postBackend("allext", path.asJson.noSpaces) { res =>
      val Right(allext) = decode[List[Node]](res)
      setState(_.copy(nodes = allext.sortBy(_.name)))
    }

  def setPath(path: String) =
    setState(_.copy(path = path))

  def reset() =
    setState(initialState)

  override def componentWillUpdate(nextProps: Props, nextState: AggregateNodeState): Unit = {
    if (nextState.path != state.path) {
      fetchAllExt(nextState.path)
    }
  }

}
