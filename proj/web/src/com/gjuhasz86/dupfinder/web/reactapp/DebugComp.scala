package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.Stat
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.NodeSelection
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import io.circe.parser._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

@react class DebugComp extends Component {
  type Props = Unit

  case class State(nodes: List[NodeLite])
  override def initialState: State = State(Nil)

  private def fetchNodes(req: NodeReq): Unit = {
    FetchUtils.postBackend("searchLite", req.asJson.noSpaces) { res =>
      val Right(nodes) = decode[List[NodeLite]](res)
      setState(_.copy(nodes = nodes))
    }
  }


  override def componentDidMount(): Unit =
    fetchNodes(NodeReq(Set("___PRESORTED___"), NodeSelection.DirectChildren, Set()))

  def render(): ReactElement = {
    div(
      div("hello"),
      state.nodes.map { node =>
        div(
          div(node.path),
          div(node.name),
          div(node.childCount),
          div(node.dupCount),
          div(node.extDupCount),
          div("-")
        )
      }
    )
  }
}