package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter._
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


case class NavNode(nodes: List[Node], selection: ChildSelection, filter: Set[ChildFilter])
object NavNode {
  val default = NavNode(List(Node.Empty), ChildSelection.Direct, Set())
}


case class NavMgrState(parents: List[NavNode], selection: ChildSelection, filter: Set[ChildFilter], fullPath: Boolean, aggregate: Boolean) {
  def current: NavNode = parents.headOption.getOrElse(NavNode.default)

  def withFilter(cf: ChildFilter) =
    this.copy(filter = filter -- NavMgrState.conflicts(cf) + cf)
}
object NavMgrState {
  private val conflicts =
    Map[ChildFilter, Set[ChildFilter]](
      Empty -> Set(NonEmpty),
      NonEmpty -> Set(Empty),
      HasExtDups -> Set(HasDups),
      HasDups -> Set(HasExtDups)
    ).withDefaultValue(Set())

}

@react class NavManager extends Component {
  case class Props(onCurrentNodeChange: NavNode => Unit, children: NavMgrState => ReactElement)
  type State = NavMgrState
  override def initialState = NavMgrState(Nil, ChildSelection.Direct, Set(), false, false)
  override def render(): ReactElement = props.children(state)

  def setFullPath(enabled: Boolean) =
    setState(_.copy(fullPath = enabled))

  def setAggregate(enabled: Boolean) =
    setState(_.copy(aggregate = enabled))

  def toggleSelection() =
    if (state.selection == ChildSelection.Deep)
      setSelection(ChildSelection.Direct)
    else
      setSelection(ChildSelection.Deep)

  def setSelection(sel: ChildSelection) =
    setState(_.copy(selection = sel))

  def toggleFilter(cf: ChildFilter) =
    if (state.filter.contains(cf)) remFilter(cf) else addFilter(cf)

  def addFilter(cf: ChildFilter) =
    setState(_.withFilter(cf))

  def remFilter(cf: ChildFilter) =
    setState(_.copy(filter = state.filter - cf))

  def root(node: Node) = setState(_.copy(parents = List(state.current.copy(nodes = List(node)))))
  def up(n: Int = 1) = {
    val newParents = state.parents.drop(n min (state.parents.size - 1))
    setState(_.copy(
      parents = newParents,
      selection = state.current.selection,
      filter = state.current.filter,
      fullPath = newParents.head.selection == ChildSelection.Deep
    ))
  }

  def downSingle(
    node: Node,
    sel: ChildSelection = state.selection,
    filter: Set[ChildFilter] = state.filter): Unit
  = down(List(node), sel, filter)

  def down(
    nodes: List[Node],
    sel: ChildSelection = state.selection,
    filters: Set[ChildFilter] = state.filter): Unit
  = {
    val ns = filters.toList.foldLeft(state)(_.withFilter(_))
    down(NavNode(nodes, sel, ns.filter))
  }

  def down(root: NavNode): Unit =
    setState(_.copy(
      parents = root :: state.parents,
      fullPath = root.selection == ChildSelection.Deep
    ))

  override def componentDidUpdate(prevProps: Props, prevState: State) =
    if (prevState.current.nodes != state.current.nodes) {
      props.onCurrentNodeChange(state.current)
    }
}
