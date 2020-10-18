package com.gjuhasz86.dupfinder.web

import io.circe.generic.extras.Configuration
import io.circe.parser._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._
import io.circe.generic.extras.auto._
import slinky.core.facade.Fragment

@react class App extends StatelessComponent {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  type Props = Unit

  val nodeWalkMgr: ReactRef[NodeWalkManager] = React.createRef[NodeWalkManager.Def]
  val aggrNodeMgr: ReactRef[AggregateNodeManager] = React.createRef[AggregateNodeManager.Def]
  val dupListMgr: ReactRef[DupListManager] = React.createRef[DupListManager.Def]
  val mainNodeList: ReactRef[NodeList] = React.createRef[NodeList.Def]
  val aggNodeList: ReactRef[NodeList] = React.createRef[NodeList.Def]

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
            onClick := (_ => loadDupHandler()))
          ("[LOAD DUPS]"),
          div(
            className := "textBtn",
            onClick := (_ => dupListMgr.current.reset())
          )("[CLEAR]"),
          dlState.richPaths.map { rp =>
            div(
              key := rp.path,
              className := (if (rp.active) "activeDup" else "")
            )(rp.path)
          }
        )
      ).withRef(dupListMgr)
    )
  }
}

