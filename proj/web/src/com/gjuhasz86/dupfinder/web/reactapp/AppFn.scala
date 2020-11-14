package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Empty
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasExtDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NodeTypeIn
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NonEmpty
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.shared.request.NodeSelection
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DeepChildren
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DirectChildren
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DupNodes
import com.gjuhasz86.dupfinder.web.FetchUtils
import io.circe.parser._
import io.circe.generic.auto._
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.Hooks._
import slinky.core.facade.React
import slinky.core.facade.ReactRef
import slinky.web.html._

import scala.collection.decorators._


@react object AppFn {

  case class State(ctxMenuActive: Boolean)
  type Props = Unit


  val component = FunctionalComponent[Props] { _ =>
    val (ctxMenuActive, setCtxMenuActive) = useState(false)

    val ctxMenu: ReactRef[Div] = React.createRef[html.Div]
    val navMgr: ReactRef[NavManager] = React.createRef[NavManager.Def]
    val selMgr: ReactRef[SelectionManager] = React.createRef[SelectionManager.Def]

    val fetchMgr = NodeFetchMgr.useChildren

    def fetchRoot(): Unit =
      FetchUtils.getBackend("rootLite") { res =>
        val Right(root) = decode[NodeLite](res)
        navMgr.current.root(root)
      }

    useEffect(() => fetchRoot(), Nil)

    div(
      onClick := (_ => setCtxMenuActive(false))
    )(
      NavManager(onCurrentNodeChange = { navNode =>
        val nr = NodeReq(navNode.nodes.map(_.path).toSet, navNode.selection, navNode.filter)
        fetchMgr.loadChildren(nr)
        selMgr.current.clear()
      })(navMgrState =>
        SelectionManager(fetchMgr.children)(selMgrState =>
          Fragment(
            div(
              className := (if (navMgrState.fullPath) "textBtn active" else "textBtn"),
              onClick := { _ => navMgr.current.setFullPath(true); fetchMgr.sortByPath() }
            )("[FULL]"),
            div(
              className := (if (!navMgrState.fullPath) "textBtn active" else "textBtn"),
              onClick := { _ => navMgr.current.setFullPath(false); fetchMgr.sortByName() }
            )("[SHORT]"),
            div(
              className := (if (navMgrState.selection == DirectChildren) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.setSelection(DirectChildren))
            )("[DIRECT]"),
            div(
              className := (if (navMgrState.selection == DeepChildren) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.setSelection(DeepChildren))
            )("[DEEP]"),
            div(
              className := (if (navMgrState.filter.contains(NodeTypeIn(Set("F")))) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.toggleFilter(NodeTypeIn(Set("F"))))
            )("[FILES]"),
            div(
              className := (if (navMgrState.filter.contains(NodeTypeIn(Set("D")))) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.toggleFilter(NodeTypeIn(Set("D"))))
            )("[DIRS]"),
            div(
              className := (if (navMgrState.filter.contains(Empty)) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.toggleFilter(Empty))
            )("[EMPTY]"),
            div(
              className := (if (navMgrState.filter.contains(NonEmpty)) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.toggleFilter(NonEmpty))
            )("[NON-EMPTY]"),
            div(
              className := "textBtn",
              onClick := (_ => selMgr.current.selectAll())
            )("[SELECTALL]"),
            //              div(
            //                className := (if (navMgrState.filter.contains(HasDups)) "textBtn active" else "textBtn"),
            //                onClick := (_ => navMgr.current.toggleFilter(HasDups))
            //              )("[DUPS]"),
            //              div(
            //                className := (if (navMgrState.filter.contains(HasExtDups)) "textBtn active" else "textBtn"),
            //                onClick := (_ => navMgr.current.toggleFilter(HasExtDups))
            //              )("[EXTDUPS]"),
            div(
              className := (if (navMgrState.aggregate) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.setAggregate(true))
            )("[AGGR]"),
            div(
              className := (if (!navMgrState.aggregate) "textBtn active" else "textBtn"),
              onClick := (_ => navMgr.current.setAggregate(false))
            )("[NOAGGR]"),
            div(className := "breadcrumbHolder")(
              navMgrState.parents.zipWithIndex.reverse.map { case (navNode, idx) =>
                div(
                  key := navNode.nodes.map(_.path).mkString(","),
                  className := "breadcrumb textBtn",
                  onClick := (_ => navMgr.current.up(idx))
                )(navNode.nodes match {
                  case node :: Nil =>
                    val deepMark = if (navNode.selection == DeepChildren) " [*]" else ""
                    s"${node.name}$deepMark"
                  case nodes =>
                    val deepMark = if (navNode.selection == DeepChildren) "*" else ""
                    s"[MULTI:${nodes.size}$deepMark]"
                })
              }.intersperse(
                div(className := "breadcrumb")(">")
              )
            ),
            div(),
            table(
              className := (if (fetchMgr.loading) "nodeTable loading" else "nodeTable")
            )(
              thead(
                tr(td("typ"), td("hash"), td("name"), td("cld"), td("dmy"), td("nzf"), td("dup"), td("ext"), td("sdc"), td("ldc"))
              ),
              tbody(
                tr(
                  hidden := navMgrState.parents.size <= 1,
                  className := "nodeRow",
                  onDoubleClick := (_ => navMgr.current.up())
                )(
                  td("D"), td(""), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
                ),
                AggrManager(fetchMgr.children, navMgrState.aggregate)(agrMgrState =>
                  agrMgrState.nodes.take(fetchMgr.limit).zipWithIndex.map { case (node, idx) =>
                    tr(
                      key := node.path,
                      className := (if (selMgrState.selected.contains(node)) "nodeRow selectedRow" else "nodeRow"),
                      onDoubleClick := {
                        case _ if node.ntype == "D" && !navMgrState.aggregate => navMgr.current.downSingle(node)
                        case _ if node.ntype == "D" && navMgrState.aggregate =>
                          navMgr.current.down(navMgrState.current.nodes, NodeSelection.DupNodes, Set(ChildFilter.DescendantOf(Set(node.path))))
                          navMgr.current.setAggregate(false)
                          navMgr.current.setFullPath(true)
                        case _ =>
                      },
                      onClick := {
                        case e if e.ctrlKey => selMgr.current.toggle(node)
                        case e if e.shiftKey => selMgr.current.addRange(idx)
                        case _ => selMgr.current.cleanAdd(node)
                      },
                      onMouseDown := { _ => selMgr.current.dragFrom(node) },
                      onMouseEnter := { case e if e.buttons == 1 => selMgr.current.dragOn(node) case _ => },
                      onMouseLeave := { case e if e.buttons == 1 => selMgr.current.dragOn(node) case _ => },
                      onContextMenu := { case e if !(e.shiftKey && e.ctrlKey) =>
                        e.preventDefault()
                        if (selMgrState.selected.isEmpty) {
                          selMgr.current.add(node)
                        }
                        ctxMenu.current.style = s"top: ${e.pageY}px; left:${e.pageX}px;"
                        setCtxMenuActive(true)
                      case e =>
                      }
                    )(
                      td(node.ntype),
                      td(node.hash),
                      td(
                        title := (if (navMgrState.fullPath) None else Some(node.path))
                      )(if (navMgrState.fullPath) node.path else node.name),
                      td(node.childCount),
                      td(node.dummyCount),
                      td(node.childFileCount),
                      td(node.dupCount),
                      td(node.extDupCount),
                      td(node.selfDupCount),
                      td(title := node.hashes.getOrElse(Set()).toList.sorted.mkString(","))(node.leafDupCount)
                    )
                  }),
              )
            ),
            div(hidden := fetchMgr.children.size <= fetchMgr.limit)(
              div(className := "textNoBtn")(s"Omitted ${fetchMgr.children.size - fetchMgr.limit} rows."),
              div(
                className := "textBtn", onClick := (_ => fetchMgr.incLimitBy(1000))
              )("[+1000]"),
              div(
                className := "textBtn", onClick := (_ => fetchMgr.noLimit())
              )(s"[+${fetchMgr.children.size - fetchMgr.limit}]"),
            ),
            div(
              hidden := !ctxMenuActive,
              className := "ctxMenu", ref := ctxMenu
            )(
              div(className := "item")(s"SELECTED (${selMgrState.selected.size})"),
              hr(className := "divider"),
              div(
                className := "item selectable",
                onClick := { _ =>
                  navMgr.current.down(selMgrState.selected.toList, DeepChildren, Set(ChildFilter.NodeTypeIn(Set("F"))))
                }
              )("CHILDREN"),
              div(
                className := "item selectable",
                onClick := { _ =>
                  navMgr.current.down(selMgrState.selected.toList, DeepChildren, Set(HasDups, NodeTypeIn(Set("F"))))
                }
              )("DUPS"),
              div(
                className := "item selectable",
                onClick := { _ =>
                  navMgr.current.down(selMgrState.selected.toList, DeepChildren, Set(HasExtDups, NodeTypeIn(Set("F"))))
                }
              )("EXT DUPS"),
              div(
                className := "item selectable",
                onClick := { _ =>
                  navMgr.current.down(selMgrState.selected.toList, DupNodes, Set())
                  fetchMgr.sortByPath()
                }
              )(s"DOUBLES (${selMgrState.selected.map(_.hash).size})"),
            )
          )
        ).withRef(selMgr)
      ).withRef(navMgr),
    )


  }
}
