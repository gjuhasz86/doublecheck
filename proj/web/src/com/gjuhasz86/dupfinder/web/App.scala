package com.gjuhasz86.dupfinder.web

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._

@react class App extends StatelessComponent {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  type Props = Unit

  val navMgr: ReactRef[NavManager] = React.createRef[NavManager.Def]
  val nodeWalkMgr: ReactRef[NodeWalkManager] = React.createRef[NodeWalkManager.Def]
  val aggrNodeMgr: ReactRef[AggregateNodeManager] = React.createRef[AggregateNodeManager.Def]
  val dupListMgr: ReactRef[DupListManager] = React.createRef[DupListManager.Def]
  val mainNodeList: ReactRef[NodeList] = React.createRef[NodeList.Def]
  val aggNodeList: ReactRef[NodeList] = React.createRef[NodeList.Def]
  val navNodeList: ReactRef[NodeList] = React.createRef[NodeList.Def]

  def fetchRoot(): Unit =
    FetchUtils.getBackend("root") { res =>
      val Right(root) = decode[Node](res)
      nodeWalkMgr.current.setRoot(root)
    }

  def onNodeClick(node: Node): Unit =
    node.ntype match {
      case "D" => nodeWalkMgr.current.downNode(node)
      case _ =>
    }

  def onExtClick(node: Node): Unit =
    node.ntype match {
      case "D" =>
        aggrNodeMgr.current.setPath(node.path)
      case _ =>
    }

  override def componentWillMount(): Unit = {
    fetchRoot()
  }

  def loadDupHandler(): Unit = {
    dupListMgr.current.setRoots(mainNodeList.current.getSelected ++ aggNodeList.current.getSelected)
  }

  def render(): ReactElement = {
    div(
      NodeWalkManager((nwState: NodeWalkState) =>
        div(
          div(nwState.current.path),
          div(className := "breadcrumbHolder")(
            nwState.parents.zipWithIndex.map { case (node, idx) =>
              Fragment(
                key := node.path,
              )(
                div(
                  className := "breadcrumb")(">"),
                div(
                  className := "breadcrumb textBtn",
                  onClick := (_ => nodeWalkMgr.current.upDir(idx))
                )(
                  node.name
                )
              )
            }.reverse,
            if (nwState.parents.nonEmpty) {
              div(
                className := "breadcrumb textBtn",
                onClick := (_ => nodeWalkMgr.current.upDir())
              )("[^]")
            } else {null}

          ),
          div(
            className := "textBtn",
            onClick := (_ => mainNodeList.current.toggleFullPath())
          )(if (mainNodeList.current != null && mainNodeList.current.state.fullPath) "[SHORT]" else "[FULL]"),
          NodeList(
            nodes = nwState.nodes,
            onNodeClick = onNodeClick _,
            onExtClick = onExtClick _,
            onSelection = (_ => aggNodeList.current.clearSelection())
          ).withRef(mainNodeList)
        )
      ).withRef(nodeWalkMgr),
      hr(),
      NavManager((nState: NavState) =>
        div(
          div(
            className := "textBtn",
            onClick := (_ => navMgr.current.toggleAutoFetch())
          )(if (nState.autoFetch) "[MANUAL]" else "[AUTO]"),
          div(
            className := "textBtn",
            onClick := (_ => if (!nState.autoFetch) {navMgr.current.forceFetch()})
          )("[LOAD]"),
          div(
            className := "textBtn",
            onClick := (_ => {
              navMgr.current.setRoots(mainNodeList.current.getSelected)
            })
          )("[LINK]"),
          if (nState.childSelection == ChildSelection.Direct)
            div(
              className := "textBtn",
              onClick := (_ => navMgr.current.setChildSelection(ChildSelection.Deep))
            )("[DEEP]")
          else
            div(
              className := "textBtn",
              onClick := (_ => navMgr.current.setChildSelection(ChildSelection.Direct))
            )("[DIRECT]"),
          div(
            className := "textBtn",
            onClick := (_ => navMgr.current.toggleFilter(ChildFilter.NonEmpty))
          )("[NONZERO]"),
          div(
            className := "textBtn",
            onClick := (_ => navMgr.current.toggleFilter(ChildFilter.Empty))
          )("[ZERO]"),
          div(
            className := "textBtn",
            onClick := (_ => navMgr.current.toggleFilter(ChildFilter.HasDups))
          )("[DUPS]"),
          div(
            className := "textBtn",
            onClick := (_ => navMgr.current.toggleFilter(ChildFilter.HasExtDups))
          )("[EXT]"),
          div(if (nState.autoFetch) "Reload mode: AUTO" else "Reload mode: MANUAL"),
          div(s"Roots: ${nState.roots.map(_.name).mkString(",")}"),
          div(s"Children selection: ${nState.childSelection}"),
          div(s"Filters: ${nState.filter.mkString(",")}"),
          NodeList(
            nodes = nState.nodes,
            onNodeClick = (e => println(s"clicked $e")),
            onExtClick = (_ => ()),
            onSelection = (_ => navNodeList.current.clearSelection())
          ).withRef(navNodeList)
        )
      ).withRef(navMgr),
      hr(),
      AggregateNodeManager((anState: AggregateNodeState) =>
        div(
          div(anState.path),
          div(
            className := "textBtn",
            onClick := (_ => aggNodeList.current.toggleFullPath())
          )(if (aggNodeList.current != null && aggNodeList.current.state.fullPath) "[SHORT]" else "[FULL]"),
          div(
            className := "textBtn",
            onClick := (_ => aggrNodeMgr.current.reset())
          )("[CLEAR]"),
          NodeList(
            nodes = anState.nodes,
            onNodeClick = (e => println(s"clicked $e")),
            onExtClick = (_ => ()),
            onSelection = (_ => mainNodeList.current.clearSelection())
          ).withRef(aggNodeList))
      ).withRef(aggrNodeMgr),
      hr(),
      DupListManager((dlState: DupListState) =>
        div(
          div(
            className := "textBtn",
            onClick := (_ => loadDupHandler())
          )("[LOAD DUPS]"),
          div(
            className := "textBtn",
            onClick := (_ => dupListMgr.current.setAggr(!dlState.aggr))
          )(if (dlState.aggr) "[NOAGGR]" else "[AGGR]"),
          div(
            className := "textBtn",
            onClick := (_ => dupListMgr.current.reset())
          )("[CLEAR]"),
          div(),
          dlState.richPaths.map { rp =>
            div(
              key := rp.path,
              className := (if (rp.active) "activeDup" else "")
            )(rp.path)
          }
        )
      ).withRef(dupListMgr),
    )
  }
}

