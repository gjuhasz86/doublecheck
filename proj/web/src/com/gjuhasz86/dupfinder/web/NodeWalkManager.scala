package com.gjuhasz86.dupfinder.web

import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class NodeWalkState(
  nodes: List[Node],
  parents: List[Node]) {
  val current = parents.headOption.getOrElse(Node.Empty)
}

@react class NodeWalkManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults
  type State = NodeWalkState
  override def render(): ReactElement = props.children(state)

  case class Props(children: NodeWalkState => ReactElement)
  override def initialState: State = NodeWalkState(Nil, Nil)


  override def componentWillUpdate(nextProps: Props, nextState: State): Unit = {
    if (nextState.current != state.current) {
      fetchChildren(nextState.current.path)
    }
  }

  private def fetchChildren(path: String): Unit =
    FetchUtils.postBackend("children", path.asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[Node]](res)
      setState(_.copy(nodes = nodes.sortBy(n => (n.ntype, n.name))))
    }

  def setRoot(path: Node): Unit =
    setState(_.copy(parents = List(path)))

  def downNode(node: Node): Unit =
    setState(_.copy(parents = node :: state.parents))

  def upDir(): Unit = upDir(1)

  def upDir(level: Int): Unit = {
    val maxLevel = Math.min(level, state.parents.size - 1)
    setState(_.copy(parents = state.parents.drop(maxLevel)))
  }

}