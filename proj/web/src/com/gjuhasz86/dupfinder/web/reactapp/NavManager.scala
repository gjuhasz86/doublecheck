package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter._
import com.gjuhasz86.dupfinder.shared.request.NodeSelection
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DeepChildren
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DupNodes
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement


case class NavNode(nodes: List[NodeLite], selection: NodeSelection, filter: Set[ChildFilter])
object NavNode {
  val default = NavNode(List(NodeLite.Empty), NodeSelection.DirectChildren, Set())
}


case class NavMgrState(parents: List[NavNode], selection: NodeSelection, filter: Set[ChildFilter], fullPath: Boolean, aggregate: Boolean) {
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
      HasDups -> Set(HasExtDups),
      NodeTypeIn(Set("D")) -> Set(NodeTypeIn(Set("F"))),
      NodeTypeIn(Set("F")) -> Set(NodeTypeIn(Set("D")))
    ).withDefaultValue(Set())

}

@react class NavManager extends Component {
  case class Props(onCurrentNodeChange: NavNode => Unit, children: NavMgrState => ReactElement)
  type State = NavMgrState
  override def initialState =
    NavMgrState(Nil, NodeSelection.DirectChildren, Set(NonEmpty), fullPath = false, aggregate = false)
  override def render(): ReactElement = props.children(state)

  def setFullPath(enabled: Boolean) =
    setState(_.copy(fullPath = enabled))

  def setAggregate(enabled: Boolean) =
    setState(_.copy(aggregate = enabled))

  def toggleSelection() =
    if (state.selection == DeepChildren)
      setSelection(NodeSelection.DirectChildren)
    else
      setSelection(DeepChildren)

  def setSelection(sel: NodeSelection) =
    setState(_.copy(selection = sel))

  def toggleFilter(cf: ChildFilter) =
    if (state.filter.contains(cf)) remFilter(cf) else addFilter(cf)

  def addFilter(cf: ChildFilter) =
    setState(_.withFilter(cf))

  def remFilter(cf: ChildFilter) =
    setState(_.copy(filter = state.filter - cf))

  def root(node: NodeLite) = setState(_.copy(parents = List(state.current.copy(nodes = List(node)))))
  def up(n: Int = 1) = {
    val newParents = state.parents.drop(n min (state.parents.size - 1))
    setState(_.copy(
      parents = newParents,
      selection = state.current.selection,
      filter = state.current.filter,
      fullPath = newParents.head.selection == DeepChildren || newParents.head.selection == DupNodes
    ))
  }

  def downSingle(
    node: NodeLite,
    sel: NodeSelection = state.selection,
    filter: Set[ChildFilter] = state.filter): Unit
  = down(List(node), sel, filter)

  def down(
    nodes: List[NodeLite],
    sel: NodeSelection = state.selection,
    filters: Set[ChildFilter] = state.filter): Unit
  = {
    val ns = filters.toList.foldLeft(state)(_.withFilter(_))
    val newNavNode = NavNode(nodes, sel, ns.filter)
    println(s"**A: $newNavNode")
    down(newNavNode)
  }

  def down(root: NavNode): Unit =
    setState(_.copy(
      parents = root :: state.parents,
      fullPath = root.selection == DeepChildren || root.selection == DupNodes
    ))

  override def componentDidUpdate(prevProps: Props, prevState: State) =
    if (prevState.current.nodes != state.current.nodes
      || (prevState.current.selection == NodeSelection.DupNodes && state.current.selection == NodeSelection.DupNodes
      && prevState.current.filter != state.current.filter)
    ) {
      props.onCurrentNodeChange(state.current)
    }
}
