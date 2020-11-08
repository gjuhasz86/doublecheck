package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

case class ChildrenMgrState(loading: Boolean, children: List[NodeLite], sorting: Ordering[NodeLite], limit: Int)

@react class ChildrenManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  case class Props(children: ChildrenMgrState => ReactElement)
  type State = ChildrenMgrState
  override def initialState = ChildrenMgrState(loading = false, Nil, Ordering.by(n => (n.ntype, n.name)), 1000)
  override def render(): ReactElement = props.children(state)

  def setLimit(n: Int) =
    setState(_.copy(limit = n))

  def incLimitBy(n: Int) =
    setState(_.copy(limit = state.limit + n))

  def noLimit() =
    setState(_.copy(limit = state.children.size))

  def sortByName() = {
    val ord: Ordering[NodeLite] = Ordering.by(n => (n.ntype, n.name))
    setState(_.copy(children = state.children.sorted(ord), sorting = ord))
  }

  def sortByPath() = {
    val ord: Ordering[NodeLite] = Ordering.by(_.path)
    setState(_.copy(children = state.children.sorted(ord), sorting = ord))
  }

  def loadChildren(req: NodeReq) =
    fetchNodes(req)

  private def fetchNodes(req: NodeReq): Unit = {
    setState(_.copy(loading = true))
    FetchUtils.postBackend("searchLite", req.asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[NodeLite]](res)
      setState(_.copy(loading = false, children = nodes.sorted(state.sorting), limit = 1000))
    }
  }
}
