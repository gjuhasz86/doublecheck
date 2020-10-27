package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._

case class ChildrenMgrState(children: List[Node]) {
}

@react class ChildrenManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults


  case class Props(children: ChildrenMgrState => ReactElement)
  type State = ChildrenMgrState
  override def initialState = ChildrenMgrState(Nil)
  override def render(): ReactElement = props.children(state)


  def setRoot(node: Node) = {
    fetchChildren(node.path)
  }

  private def fetchChildren(path: String): Unit =
    FetchUtils.postBackend("children", path.asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[Node]](res)
      setState(_.copy(children = nodes.sortBy(n => (n.ntype, n.name))))
    }
}
