package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Empty
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasExtDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NodeTypeIn
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NonEmpty
import com.gjuhasz86.dupfinder.shared.request.NodeReq
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

  type Props = Unit

  val component = FunctionalComponent[Props] { _ =>
    val (ctxMenuActive, setCtxMenuActive) = useState(false)

    val ctxMenu: ReactRef[Div] = React.createRef[html.Div]

    val fetchMgr = FetchMgr.useChildren
    val selMgr = SelMgr.useSelection(fetchMgr.children)
    val navMgr = NavMgr.useNavigation(onCurrentChange = navNode => {
      val nr = NodeReq(navNode.navNode.nodes.map(_.path).toSet, navNode.navNode.selection, navNode.navNode.filter)
      fetchMgr.loadChildren(nr)
      selMgr.clear()
    })
    val aggrNodes = AggrMgr.useAggregation(fetchMgr.children, navMgr.current.viewNode.aggregate)

    def fetchRoot(): Unit =
      FetchUtils.getBackend("rootLite") { res =>
        val Right(root) = decode[NodeLite](res)
        navMgr.root(root)
      }

    useEffect(() => fetchRoot(), Nil)

    div(
      onClick := (_ => setCtxMenuActive(false))
    )(
      Fragment(
        div(
          TextButton(
            active = navMgr.nextNavNode.viewNode.fullPath,
            clickHandler = { _ =>
              navMgr.changeNextNavNode(_.setFullPath(true))
              navMgr.changeCurrNavNode(_.setFullPath(true))
              fetchMgr.sortByPath()
            }
          )("[FULL]"),
          div(
            className := (if (!navMgr.nextNavNode.viewNode.fullPath) "textBtn active" else "textBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setFullPath(false))
              navMgr.changeCurrNavNode(_.setFullPath(false))
              fetchMgr.sortByName()
            }
          )("[SHORT]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.selection == DirectChildren) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.setSelection(DirectChildren)))
          )("[DIRECT]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.selection == DeepChildren) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.setSelection(DeepChildren)))
          )("[DEEP]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.selection == DupNodes) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.setSelection(DupNodes)))
          )("[DOUBLES]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NodeTypeIn(Set("F")))) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NodeTypeIn(Set("F")))))
          )("[FILES]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NodeTypeIn(Set("D")))) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NodeTypeIn(Set("D")))))
          )("[DIRS]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(Empty)) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(Empty)))
          )("[EMPTY]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NonEmpty)) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NonEmpty)))
          )("[NON-EMPTY]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(HasDups)) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(HasDups)))
          )("[DUPS]"),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(HasExtDups)) "textBtn active" else "textBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(HasExtDups)))
          )("[EXTDUPS]"),
          div(
            className := (if (navMgr.nextNavNode.viewNode.aggregate) "textBtn active" else "textBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setAggregate(true))
              navMgr.changeCurrNavNode(_.setAggregate(true))
            }
          )("[AGGR]"),
          div(
            className := (if (!navMgr.nextNavNode.viewNode.aggregate) "textBtn active" else "textBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setAggregate(false))
              navMgr.changeCurrNavNode(_.setAggregate(false))
            }
          )("[NOAGGR]"),
          div(
            className := "textBtn",
            onClick := (_ => selMgr.selectAll())
          )("[SELECTALL]")
        ),
        div(
          div(
            className := (if (navMgr.current.viewNode.fullPath) "textBtn indicator active" else "textBtn indicator")
          )("[FULL]"),
          div(
            className := (if (!navMgr.current.viewNode.fullPath) "textBtn indicator active" else "textBtn indicator")
          )("[SHORT]"),
          div(
            className := (if (navMgr.current.navNode.selection == DirectChildren) "textBtn indicator active" else "textBtn indicator")
          )("[DIRECT]"),
          div(
            className := (if (navMgr.current.navNode.selection == DeepChildren) "textBtn indicator active" else "textBtn indicator")
          )("[DEEP]"),
          div(
            className := (if (navMgr.current.navNode.selection == DupNodes) "textBtn indicator active" else "textBtn indicator")
          )("[DOUBLES]"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(NodeTypeIn(Set("F")))) "textBtn indicator active" else "textBtn indicator")
          )("[FILES]"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(NodeTypeIn(Set("D")))) "textBtn indicator active" else "textBtn indicator")
          )("[DIRS]"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(Empty)) "textBtn indicator active" else "textBtn indicator")
          )("[EMPTY]"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(NonEmpty)) "textBtn indicator active" else "textBtn indicator")
          )("[NON-EMPTY]"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(HasDups)) "textBtn indicator active" else "textBtn indicator")
          )("[DUPS]"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(HasExtDups)) "textBtn indicator active" else "textBtn indicator")
          )("[EXTDUPS]"),
          div(
            className := (if (navMgr.current.viewNode.aggregate) "textBtn indicator active" else "textBtn indicator")
          )("[AGGR]"),
          div(
            className := (if (!navMgr.current.viewNode.aggregate) "textBtn indicator active" else "textBtn indicator")
          )("[NOAGGR]"),
          div(
            className := "textBtn",
            onClick := (_ => navMgr.syncNext())
          )("[SYNC]")
        ),
        div(className := "breadcrumbHolder")(
          navMgr.parents.zipWithIndex.reverse.map { case (FullNavNode(navNode, viewNode), idx) =>
            div(
              key := navNode.nodes.map(_.path).mkString(","),
              className := "breadcrumb textBtn",
              onClick := (_ => navMgr.up(idx)),
              title := s"${viewNode.manual}"
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
              hidden := navMgr.parents.size <= 1,
              className := "nodeRow",
              onDoubleClick := (_ => navMgr.up())
            )(
              td("D"), td(""), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
            ),
            aggrNodes.take(fetchMgr.limit).zipWithIndex.map { case (node, idx) =>
              tr(
                key := node.path,
                className := (if (selMgr.selected.contains(node)) "nodeRow selectedRow" else "nodeRow"),
                onDoubleClick := {
                  case _ if node.ntype == "D" && !navMgr.nextNavNode.viewNode.aggregate => navMgr.down(node)
                  case _ if node.ntype == "D" && navMgr.nextNavNode.viewNode.aggregate =>
                    navMgr.down(
                      navMgr.current.navNode.nodes,
                      navMgr.current.addFilter(ChildFilter.DescendantOf(Set(node.path)))
                        .setAggregate(false)
                        .setFullPath(true)
                    )
                  case _ =>
                },
                onClick := {
                  case e if e.ctrlKey => selMgr.toggle(node)
                  case e if e.shiftKey => selMgr.addRange(idx)
                  case _ => selMgr.cleanAdd(node)
                },
                onMouseDown := { _ => selMgr.dragFrom(node) },
                onMouseEnter := { case e if e.buttons == 1 => selMgr.dragOn(node) case _ => },
                onMouseLeave := { case e if e.buttons == 1 => selMgr.dragOn(node) case _ => },
                onContextMenu := { case e if !(e.shiftKey && e.ctrlKey) =>
                  e.preventDefault()
                  if (selMgr.selected.isEmpty) {
                    selMgr.add(node)
                  }
                  ctxMenu.current.style = s"top: ${e.pageY}px; left:${e.pageX}px;"
                  setCtxMenuActive(true)
                case _ =>
                }
              )(
                td(node.ntype),
                td(node.hash),
                td(
                  title := (if (navMgr.current.viewNode.fullPath) None else Some(node.path))
                )(if (navMgr.current.viewNode.fullPath) node.path else node.name),
                td(node.childCount),
                td(node.dummyCount),
                td(node.childFileCount),
                td(node.dupCount),
                td(node.extDupCount),
                td(node.selfDupCount),
                td(title := node.hashes.getOrElse(Set()).toList.sorted.mkString(","))(node.leafDupCount)
              )
            }
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
          className := "ctxMenu",
          ref := ctxMenu
        )(
          div(className := "item")(s"SELECTED (${selMgr.selected.size})"),
          hr(className := "divider"),
          div(
            className := "item selectable",
            onClick := { _ =>
              navMgr.down(
                selMgr.selected.toList,
                navMgr.nextNavNode.setSelection(DeepChildren)
                  .addFilter(ChildFilter.NodeTypeIn(Set("F")))
                  .setManual(false)
              )
            }
          )("CHILDREN"),
          div(
            className := "item selectable",
            onClick := { _ =>
              navMgr.down(
                selMgr.selected.toList,
                navMgr.nextNavNode.setSelection(DeepChildren)
                  .addFilter(HasDups)
                  .addFilter(NodeTypeIn(Set("F")))
                  .setManual(false)
              )
            }
          )("DUPS"),
          div(
            className := "item selectable",
            onClick := { _ =>
              navMgr.down(
                selMgr.selected.toList,
                navMgr.nextNavNode.setSelection(DeepChildren)
                  .setFullPath(true)
                  .addFilter(HasExtDups)
                  .addFilter(NodeTypeIn(Set("F")))
                  .setManual(false)
              )
              fetchMgr.sortByPath()
            }
          )("EXT DUPS"),
          div(
            className := "item selectable",
            onClick := { _ =>
              navMgr.down(selMgr.selected.toList,
                navMgr.nextNavNode.setSelection(DupNodes)
                  .setAggregate(true)
                  .setFullPath(true)
                  .setManual(false))
              fetchMgr.sortByPath()
            }
          )(s"DOUBLES (${selMgr.selected.map(_.hash).size})"),
        )
      )
    )


  }
}
