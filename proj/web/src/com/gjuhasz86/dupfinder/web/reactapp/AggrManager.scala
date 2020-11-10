package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import io.circe.generic.extras.Configuration
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement

import scala.annotation.tailrec

case class AggrManagerState(nodes: List[NodeLite])
@react class AggrManager extends Component {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  case class Props(nodes: List[NodeLite], enabled: Boolean, children: AggrManagerState => ReactElement)
  type State = AggrManagerState
  override def initialState = AggrManagerState(Nil)
  override def render(): ReactElement = props.children(state)

  override def componentDidUpdate(prevProps: Props, prevState: AggrManagerState): Unit = {
    if (prevProps.nodes != props.nodes || prevProps.enabled != props.enabled) {
      setState(_.copy(nodes = aggregate(props.nodes)))
    }
  }

  def aggregate(nodes: List[NodeLite]): List[NodeLite] = {
    @tailrec
    def norm(list: List[NodeLite], acc: List[NodeLite]): List[NodeLite] =
      list match {
        case a :: rest if a.ntype != "D" => norm(rest, acc)
        case a :: b :: rest if a.hashes == b.hashes => norm(b :: rest, acc)
        case a :: rest => norm(rest, a :: acc)
        case Nil => acc
      }

    if (props.enabled)
      norm(nodes.sortBy(_.path), Nil).reverse
    else
      nodes
  }

}
