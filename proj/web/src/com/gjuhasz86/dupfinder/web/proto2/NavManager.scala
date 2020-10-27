package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class NavMgrState(parents: List[Node]) {
  def current: Node = parents.headOption.getOrElse(Node.Empty)
}

@react class NavManager extends Component {
  case class Props(onCurrentNodeChange: Node => Unit, children: NavMgrState => ReactElement)
  type State = NavMgrState
  override def initialState = NavMgrState(Nil)
  override def render(): ReactElement = props.children(state)

  def root(node: Node) = setState(_.copy(List(node)))
  def up(n: Int = 1) = setState(_.copy(state.parents.drop(n min (state.parents.size - 1))))
  def down(node: Node) = setState(_.copy(node :: state.parents))

  override def componentDidUpdate(prevProps: Props, prevState: State) = {
    if (prevState.current != state.current) {
      props.onCurrentNodeChange(state.current)
    }
  }
}
