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

case class ChildrenMgrState() {
}

@react class ChildrenManager extends Component {
  case class Props(children: ChildrenMgrState => ReactElement)
  type State = ChildrenMgrState
  override def initialState = ChildrenMgrState()
  override def render(): ReactElement = props.children(state)

}
