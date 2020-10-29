package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Empty
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasExtDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NonEmpty
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.shared.request.ChildSelection.Deep
import com.gjuhasz86.dupfinder.shared.request.ChildSelection.Direct
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._

import scala.collection.decorators._

@react class App2 extends Component {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  def fetchRoot(): Unit =
    FetchUtils.getBackend("root") { res =>
      val Right(root) = decode[Node](res)
      navMgr.current.root(root)
    }

  type Props = Unit
  case class State(ctxMenuActive: Boolean)
  override def initialState: State = State(false)

  val ctxMenu: ReactRef[Div] = React.createRef[html.Div]
  val navMgr: ReactRef[NavManager] = React.createRef[NavManager.Def]
  val chldMgr: ReactRef[ChildrenManager] = React.createRef[ChildrenManager.Def]
  val selMgr: ReactRef[SelectionManager] = React.createRef[SelectionManager.Def]


  override def componentDidMount(): Unit = {
    fetchRoot()
  }

  def render(): ReactElement = {
    div(
      onClick := (_ => setState(_.copy(ctxMenuActive = false)))
    )(
      NavManager(onCurrentNodeChange = { navNode =>
        chldMgr.current.loadChildren(NodeReq(navNode.nodes.map(_.path).toSet, navNode.selection, navNode.filter))
        selMgr.current.clear()
      })(navMgrState =>
        ChildrenManager(chldMgrState =>
          SelectionManager(chldMgrState.children)(selMgrState =>
            Fragment(
              div(
                className := (if (navMgrState.fullPath) "textBtn active" else "textBtn"),
                onClick := { _ => navMgr.current.setFullPath(true); chldMgr.current.sortByPath() }
              )("[FULL]"),
              div(
                className := (if (!navMgrState.fullPath) "textBtn active" else "textBtn"),
                onClick := { _ => navMgr.current.setFullPath(false); chldMgr.current.sortByName() }
              )("[SHORT]"),
              div(
                className := (if (navMgrState.selection == Direct) "textBtn active" else "textBtn"),
                onClick := (_ => navMgr.current.setSelection(Direct))
              )("[DIRECT]"),
              div(
                className := (if (navMgrState.selection == Deep) "textBtn active" else "textBtn"),
                onClick := (_ => navMgr.current.setSelection(Deep))
              )("[DEEP]"),
              div(
                className := (if (navMgrState.filter.contains(Empty)) "textBtn active" else "textBtn"),
                onClick := (_ => navMgr.current.toggleFilter(Empty))
              )("[EMPTY]"),
              div(
                className := (if (navMgrState.filter.contains(NonEmpty)) "textBtn active" else "textBtn"),
                onClick := (_ => navMgr.current.toggleFilter(NonEmpty))
              )("[NON-EMPTY]"),
              div(
                className := (if (navMgrState.filter.contains(HasDups)) "textBtn active" else "textBtn"),
                onClick := (_ => navMgr.current.toggleFilter(HasDups))
              )("[DUPS]"),
              div(
                className := (if (navMgrState.filter.contains(HasExtDups)) "textBtn active" else "textBtn"),
                onClick := (_ => navMgr.current.toggleFilter(HasExtDups))
              )("[EXTDUPS]"),
              div(className := "breadcrumbHolder")(
                navMgrState.parents.zipWithIndex.reverse.map { case (navNode, idx) =>
                  div(
                    key := navNode.nodes.map(_.path).mkString(","),
                    className := "breadcrumb textBtn",
                    onClick := (_ => navMgr.current.up(idx))
                  )(navNode.nodes match {
                    case node :: Nil =>
                      val deepMark = if (navNode.selection == Deep) " [*]" else ""
                      s"${node.name}$deepMark"
                    case nodes =>
                      val deepMark = if (navNode.selection == Deep) "*" else ""
                      s"[MULTI:${nodes.size}$deepMark]"
                  })
                }.intersperse(
                  div(className := "breadcrumb")(">")
                )
              ),
              div(),
              table(
                className := (if (chldMgrState.loading) "nodeTable loading" else "nodeTable")
              )(
                thead(
                  tr(td("typ"), td("name"), td("cld"), td("dmy"), td("nzf"), td("dup"), td("ext"), td("sdc"))
                ),
                tbody(
                  tr(
                    hidden := navMgrState.parents.size <= 1 || chldMgrState.loading,
                    className := "nodeRow",
                    onDoubleClick := (_ => navMgr.current.up())
                  )(
                    td("D"), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
                  ),
                  chldMgrState.children.take(chldMgrState.limit).zipWithIndex.map { case (node, idx) =>
                    tr(
                      key := node.path,
                      className := (if (selMgrState.selected.contains(node)) "nodeRow selectedRow" else "nodeRow"),
                      onDoubleClick := { case _ if node.ntype == "D" => navMgr.current.downSingle(node) case _ => },
                      onClick := {
                        case e if e.ctrlKey => selMgr.current.toggle(node)
                        case e if e.shiftKey => selMgr.current.addRange(idx)
                        case _ => selMgr.current.cleanAdd(node)
                      },
                      onMouseDown := { _ => selMgr.current.dragFrom(node) },
                      onMouseEnter := { case e if e.buttons == 1 => selMgr.current.dragOn(node) case _ => },
                      onMouseLeave := { case e if e.buttons == 1 => selMgr.current.dragOn(node) case _ => },
                      onContextMenu := { e =>
                        e.preventDefault()
                        if (selMgrState.selected.isEmpty) {
                          selMgr.current.add(node)
                        }
                        ctxMenu.current.style = s"top: ${e.pageY}px; left:${e.pageX}px;"
                        setState(_.copy(ctxMenuActive = true))
                      }
                    )(
                      td(node.ntype),
                      td(
                        title := (if (navMgrState.fullPath) None else Some(node.path))
                      )(if (navMgrState.fullPath) node.path else node.name),
                      td(node.childCount),
                      td(node.dummyCount),
                      td(node.stats.getOrElse[Int]("F", 0)),
                      td(node.dupCount),
                      td(node.extDupCount),
                      td(node.selfDupCount)
                    )
                  }
                )
              ),
              div(hidden := chldMgrState.children.size <= chldMgrState.limit)(
                div(className := "textNoBtn")(s"Omitted ${chldMgrState.children.size - chldMgrState.limit} rows."),
                div(
                  className := "textBtn", onClick := (_ => chldMgr.current.incLimitBy(1000))
                )("[+1000]"),
                div(
                  className := "textBtn", onClick := (_ => chldMgr.current.noLimit())
                )(s"[+${chldMgrState.children.size - chldMgrState.limit}]"),
              ),
              div(
                hidden := !state.ctxMenuActive,
                className := "ctxMenu", ref := ctxMenu
              )(
                div(className := "item")(s"SELECTED (${selMgrState.selected.size})"),
                hr(className := "divider"),
                div(
                  className := "item selectable",
                  onClick := { _ =>
                    navMgr.current.down(selMgrState.selected.toList, ChildSelection.Direct)
                  }
                )("DIRECT CHILDREN"),
                div(
                  className := "item selectable",
                  onClick := { _ =>
                    navMgr.current.down(selMgrState.selected.toList, Deep, Set(ChildFilter.NodeTypeIn(Set("F"))))
                  }
                )("DEEP CHILDREN"),
                div(
                  className := "item selectable",
                  onClick := { _ =>
                    navMgr.current.down(selMgrState.selected.toList, Deep, Set(HasDups))
                  }
                )("DUPS"),
                div(
                  className := "item selectable",
                  onClick := { _ =>
                    navMgr.current.down(selMgrState.selected.toList, Deep, Set(HasExtDups))
                  }
                )("EXT DUPS"),
              )
            )
          ).withRef(selMgr)
        ).withRef(chldMgr)
      ).withRef(navMgr)
    )
  }
}