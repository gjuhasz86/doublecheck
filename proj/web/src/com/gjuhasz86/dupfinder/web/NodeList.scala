package com.gjuhasz86.dupfinder.web


import slinky.core._
import slinky.core.annotations.react
import slinky.web.SyntheticMouseEvent
import slinky.web.html._

@react class NodeList extends Component {

  case class Props(
    nodes: List[Node],
    onNodeClick: Node => Unit,
    onExtClick: Node => Unit,
    onSelection: Unit => Unit,
  )
  case class State(lastSelectedIndex: Int, selection: Set[Int], fullPath: Boolean)

  override def initialState: State = State(0, Set(), false)

  def rowClass(ntype: String, idx: Int): String = {
    if (state.selection.contains(idx))
      "nodeRow selectedRow"
    else
      "nodeRow"
  }

  def getSelected: List[Node] =
    props.nodes.zipWithIndex.collect { case (node, idx) if state.selection.contains(idx) => node }

  def toggleFullPath() = setState(_.copy(fullPath = !state.fullPath))

  def setFullPath(fp: Boolean) = setState(_.copy(fullPath = fp))

  private def handleNodeClick(e: SyntheticMouseEvent[_], idx: Int, node: Node): Unit = {
    e.stopPropagation()
    if (e.shiftKey) {
      val List(min, max) = List(state.lastSelectedIndex, idx).sorted
      setState(_.copy(lastSelectedIndex = idx, selection = state.selection ++ (min to max)))
    } else if (e.ctrlKey && state.selection.contains(idx)) {
      setState(_.copy(lastSelectedIndex = idx, selection = state.selection - idx))
    } else if (e.ctrlKey) {
      setState(_.copy(lastSelectedIndex = idx, selection = state.selection + idx))
    } else {
      setState(_.copy(lastSelectedIndex = idx, selection = Set(idx)))
      props.onNodeClick(node)
    }
    props.onSelection.apply()
  }

  def clearSelection() = {
    setState(_.copy(lastSelectedIndex = 0, selection = Set()))
  }

  override def componentWillUpdate(nextProps: Props, nextState: State): Unit = {
    if (nextProps.nodes != props.nodes) {
      setState(_.copy(lastSelectedIndex = 0, selection = Set()))
    }
  }

  def render() = {
    table()(
      thead()(
        tr()(
          td()("typ"),
          td()("name"),
          td()("cld"),
          td()("dmy"),
          td()("nzf"),
          td()("dup"),
          td()("ext"),
          td()("sdc"),
        )
      ),
      tbody()(
        props.nodes.zipWithIndex.map { case (node, idx) =>
          tr(
            key := node.path,
            className := rowClass(node.ntype, idx),
            onClick := (e => handleNodeClick(e, idx, node))
          )(
            td()(node.ntype),
            td(title := node.path)(if (state.fullPath) node.path else node.name),
            td()(node.childCount),
            td()(node.dummyCount),
            td()(node.stats.getOrElse("F", 0): Int),
            td()(node.dupCount),
            td(
              className := "extCell",
              onClick := (e => {e.stopPropagation(); props.onExtClick(node)})
            )(node.extDupCount),
            td()(node.selfDupCount),
          )
        }
      )
    )
  }
}
