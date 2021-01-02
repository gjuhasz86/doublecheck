package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.Stat
import com.gjuhasz86.dupfinder.shared.StatKey
import com.gjuhasz86.dupfinder.shared.Stats
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Active
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Deleted
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Empty
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasExtDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Ignored
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Inactive
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NoSafeDup
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NodeTypeIn
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NonEmpty
import com.gjuhasz86.dupfinder.shared.request.MarkReq
import com.gjuhasz86.dupfinder.shared.request.MarkType
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DeepChildren
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DirectChildren
import com.gjuhasz86.dupfinder.shared.request.NodeSelection.DupNodes
import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.reactapp.ColMgr.ColHeader
import com.gjuhasz86.dupfinder.web.Utils.skDec
import com.gjuhasz86.dupfinder.web.Utils.skEnc
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import rx._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.Hooks._
import slinky.core.facade.React
import slinky.core.facade.ReactRef
import slinky.web.html._

import scala.collection.decorators._

@react object Panel {
  type ColDef = ColMgr.ColDef[NodeLite]

  case class Props(id: Int, className: String, onSelectionChange: List[NodeLite] => Unit,
    inputNodes: Rx[List[NodeLite]], inputSettings: Rx[FullNavNode])

  val allColumns: List[ColDef] = List(
    NodeStatCol("typ", Stat.NType)(_.value.short),
    NodeStatCol("hash", Stat.Hash)(_.value),
    NodeStatCol("name", Stat.Name)(_.value),
    NodeStatCol("size", Stat.Size)(_.value),
    NodeStatCol("cld", Stat.ChildCount)(_.value),
    NodeStatCol("dir", Stat.ChildDirCount)(_.value),
    NodeStatCol("zfc", Stat.EmptyFileCount)(_.value),
    NodeStatCol("nzf", Stat.ChildFileCount)(_.value),
    NodeStatCol("dmy", Stat.DummyCount)(_.value),
    NodeStatCol("dup", Stat.DupCount)(_.value),
    NodeStatCol("ext", Stat.ExtDupCount)(_.value),
    NodeStatCol("sdc", Stat.SelfDupCount)(_.value),
    NodeStatCol("saf", Stat.SafeDupCount)(_.value),
    NodeStatCol("ldc", Stat.LeafDupCount)(_.value)
  )

  val hiddenCols = Set("hash", "dmy")
  val allowWrap = Set("name")

  val colMap: Map[String, ColDef] =
    allColumns.map(c => c.name -> c).toMap

  val defaultCols =
    allColumns.map(c => ColHeader(c.name, !hiddenCols.contains(c.name)))

  val component = FunctionalComponent[Props] { props =>
    val (ctxMenuActive, setCtxMenuActive) = useState(false)
    val (headerMenuActive, setHeaderMenuActive) = useState(false)
    val (dragCol, setDragCol) = useState(None: Option[String])
    val (dragColOver, setDragColOver) = useState(None: Option[String])

    val ctxMenu: ReactRef[Div] = React.createRef[html.Div]
    val headerMenu: ReactRef[Div] = React.createRef[html.Div]

    val fetchMgr = FetchMgr.useChildren
    val selMgr = SelMgr.useSelection(fetchMgr.children)

    useEffect(() => {
      props.onSelectionChange(selMgr.selected.toList)
    }, List(selMgr.selected)
    )

    val navMgr = NavMgr.useNavigation(onCurrentChange = navNode => {
      val nr = NodeReq(navNode.navNode.nodes.map(_.path).toSet, navNode.navNode.selection, navNode.navNode.filter)
      fetchMgr.loadChildren(nr)
      selMgr.clear()
    })

    val colMgr = ColMgr.useColumns(
      if (navMgr.current.viewNode.fullPath) colMap + ("name" -> NodeCol.of("name")(_.path)) else colMap
    )(defaultCols)

    val dragIdx = dragCol.map(c => colMgr.columns.map(_.name).indexOf(c)).getOrElse(0)

    val aggrNodes = AggrMgr.useAggregation(fetchMgr.children, navMgr.current.viewNode.aggregate)

    def fetchRoot(): Unit =
      FetchUtils.getBackend("rootLite") { res =>
        val Right(root) = decode[NodeLite](res)
        navMgr.root(root)
      }

    def mark(req: MarkReq): Unit = {
      FetchUtils.postBackend("mark", req.asJson.noSpaces) { res =>
        val Right(markedPaths) = decode[List[NodeLite]](res)
        println(markedPaths)
      }
    }

    useEffect(() => fetchRoot(), Nil)

    useEffect { () =>
      val obs = props.inputNodes.triggerLater { nodes =>
        //        println(s"Triggered Obs in [#${props.id}]")
        if (navMgr.parents.size > 1) {
          navMgr.changeCurrNavNode(_.setNodes(nodes))
        } else {
          navMgr.down(nodes)
        }
      }
      () => obs.kill()
    }

    useEffect { () =>
      val obs = props.inputSettings.triggerLater { navNode =>
        println(s"Triggered Obs in [#${props.id}] [$navNode]")
        navMgr.changeCurrNavNode(nn => navNode.setNodes(nn.navNode.nodes))
        navMgr.changeNextNavNode(nn => navNode.setNodes(nn.navNode.nodes))
      }
      () => obs.kill()
    }

    val (hideMenu, setHideMenu) = useState(false)

    div(
      className := props.className,
      onClick := { _ => println("ON OUTER CLICK"); setCtxMenuActive(false); setHeaderMenuActive(false) }
    )(
      Fragment(
        /* ----------------- NEXT TOOLBAR ---------------- */
        div(className := (if (hideMenu) "hiddenMenu" else ""))(
          div(
            className := "iconBtn hideMenu",
            onClick := { _ => setHideMenu(!_) }
          )(
            if (hideMenu)
              span(title := "SHOW", className := "mdi mdi-arrow-expand-right")
            else
              span(title := "HIDE", className := "mdi mdi-arrow-expand-left")
          ),

          div(
            className := "iconBtn",
            onClick := (_ => navMgr.syncCurr())
          )(span(title := "SYNC-DOWN", className := "mdi mdi-arrow-collapse-down")),

          div(className := "iconBtn separator"),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.nextNavNode.navNode.selection == DirectChildren) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.setSelection(DirectChildren)))
          )(span(title := "DIRECT", className := "mdi mdi-layers-outline")), // mdi-layers-outline mdi-animation-outline mdi-folder-open-outline mdi-folder-open-outline mdi-format-list-bulleted-square mdi-rhombus-outline
          div(
            className := (if (navMgr.nextNavNode.navNode.selection == DeepChildren) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.setSelection(DeepChildren)))
          )(span(title := "DEEP", className := "mdi mdi-layers-triple-outline")),
          div(
            className := (if (navMgr.nextNavNode.navNode.selection == DupNodes) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.setSelection(DupNodes)))
          )(span(title := "DOUBLES", className := "mdi mdi-checkbox-multiple-blank-outline")),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NodeTypeIn(Set("F")))) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NodeTypeIn(Set("F")))))
          )(span(title := "FILES", className := "mdi mdi-file-outline")),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NodeTypeIn(Set("D")))) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NodeTypeIn(Set("D")))))
          )(span(title := "DIRS", className := "mdi mdi-folder-outline")),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(Empty)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(Empty)))
          )(span(title := "EMPTY", className := "mdi mdi-tray")),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NonEmpty)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NonEmpty)))
          )(span(title := "NON-EMPTY", className := "mdi mdi-tray-full")),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(HasDups)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(HasDups)))
          )(span(title := "DUPS", className := "mdi mdi-file-multiple-outline")),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(HasExtDups)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(HasExtDups)))
          )(span(title := "EXTDUPS", className := "mdi mdi-folder-multiple-outline")),

          div(className := "iconBtn separator"),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(Ignored)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(Ignored)))
          )(span(title := "IGNORED", className := "mdi mdi-cancel")), // mdi-cancel mdi-vanish
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(Deleted)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(Deleted)))
          )(span(title := "DELETED", className := "mdi mdi-file-excel-box-outline")),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(Inactive)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(Inactive)))
          )(span(title := "INACTIVE", className := "mdi mdi-delete-outline")),
          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(Active)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(Active)))
          )(span(title := "ACTIVE", className := "mdi mdi-delete-off-outline")),

          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.nextNavNode.navNode.filter.contains(NoSafeDup)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeNextNavNode(_.toggleFilter(NoSafeDup)))
          )(span(title := "NOSAFE", className := "mdi mdi-select-multiple")),

          div(className := "iconBtn separator"),
          div(className := "iconBtn separator"),

          div(
            className := (if (!navMgr.nextNavNode.viewNode.fullPath) "iconBtn active" else "iconBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setFullPath(false))
              navMgr.changeCurrNavNode(_.setFullPath(false))
              fetchMgr.sortByName()
            }
          )(span(title := "SHORT", className := "mdi mdi-unfold-more-vertical")), // mdi-contain mdi-unfold-more-vertical
          div(
            className := (if (navMgr.nextNavNode.viewNode.fullPath) "iconBtn active" else "iconBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setFullPath(true))
              navMgr.changeCurrNavNode(_.setFullPath(true))
              fetchMgr.sortByPath()
            }
          )(span(title := "FULL", className := "mdi mdi-xml")), // mdi-pan-horizontal mdi-xml
          div(className := "iconBtn separator"),

          div(
            className := (if (!navMgr.nextNavNode.viewNode.aggregate) "iconBtn active" else "iconBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setAggregate(false))
              navMgr.changeCurrNavNode(_.setAggregate(false))
            }
          )(span(title := "NOAGGR", className := "mdi mdi-format-list-bulleted-square")),
          div(
            className := (if (navMgr.nextNavNode.viewNode.aggregate) "iconBtn active" else "iconBtn"),
            onClick := { _ =>
              navMgr.changeNextNavNode(_.setAggregate(true))
              navMgr.changeCurrNavNode(_.setAggregate(true))
            }
          )(span(title := "AGGR", className := "mdi mdi-file-tree")),
          div(className := "iconBtn separator"),

          div(
            className := "iconBtn",
            onClick := (_ => selMgr.selectAll())
          )(span(title := "SELECTALL", className := "mdi mdi-asterisk"))
        ),

        /* ----------------- CURRENT TOOLBAR ---------------- */

        div(className := (if (hideMenu) "hiddenMenu" else ""))(
          div(
            className := "iconBtn hideMenu",
            onClick := { _ => setHideMenu(!_) }
          )(
            if (hideMenu)
              span(title := "SHOW", className := "mdi mdi-arrow-expand-right")
            else
              span(title := "HIDE", className := "mdi mdi-arrow-expand-left")
          ),

          div(
            className := "iconBtn",
            onClick := (_ => navMgr.syncNext())
          )(span(title := "SYNC-UP", className := "mdi mdi-arrow-collapse-up")),

          div(className := "iconBtn separator"),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.current.navNode.selection == DirectChildren) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.setSelection(DirectChildren)))
          )(span(title := "DIRECT", className := "mdi mdi-layers-outline")),
          div(
            className := (if (navMgr.current.navNode.selection == DeepChildren) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.setSelection(DeepChildren)))
          )(span(title := "DEEP", className := "mdi mdi-layers-triple-outline")),
          div(
            className := (if (navMgr.current.navNode.selection == DupNodes) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.setSelection(DupNodes)))
          )(span(title := "DOUBLES", className := "mdi mdi-checkbox-multiple-blank-outline")),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.current.navNode.filter.contains(NodeTypeIn(Set("F")))) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(NodeTypeIn(Set("F")))))
          )(span(title := "FILES", className := "mdi mdi-file-outline")),
          div(
            className := (if (navMgr.current.navNode.filter.contains(NodeTypeIn(Set("D")))) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(NodeTypeIn(Set("D")))))
          )(span(title := "DIRS", className := "mdi mdi-folder-outline")),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.current.navNode.filter.contains(Empty)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(Empty)))
          )(span(title := "EMPTY", className := "mdi mdi-tray")),
          div(
            className := (if (navMgr.current.navNode.filter.contains(NonEmpty)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(NonEmpty)))
          )(span(title := "NON-EMPTY", className := "mdi mdi-tray-full")),
          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.current.navNode.filter.contains(HasDups)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(HasDups)))
          )(span(title := "DUPS", className := "mdi mdi-file-multiple-outline")),
          div(
            className := (if (navMgr.current.navNode.filter.contains(HasExtDups)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(HasExtDups)))
          )(span(title := "EXTDUPS", className := "mdi mdi-folder-multiple-outline")),

          div(className := "iconBtn separator"),
          div(className := "iconBtn separator"),
          div(
            className := (if (navMgr.current.navNode.filter.contains(Ignored)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(Ignored)))
          )(span(title := "IGNORED", className := "mdi mdi-cancel")), // mdi-cancel mdi-vanish
          div(
            className := (if (navMgr.current.navNode.filter.contains(Deleted)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(Deleted)))
          )(span(title := "DELETED", className := "mdi mdi-file-excel-box-outline")),
          div(
            className := (if (navMgr.current.navNode.filter.contains(Inactive)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(Inactive)))
          )(span(title := "INACTIVE", className := "mdi mdi-delete-outline")), // mdi-delete-forever-outline
          div(
            className := (if (navMgr.current.navNode.filter.contains(Active)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(Active)))
          )(span(title := "ACTIVE", className := "mdi mdi-delete-off-outline")),

          div(className := "iconBtn separator"),

          div(
            className := (if (navMgr.current.navNode.filter.contains(NoSafeDup)) "iconBtn active" else "iconBtn"),
            onClick := (_ => navMgr.changeCurrNavNode(_.toggleFilter(NoSafeDup)))
          )(span(title := "NOSAFE", className := "mdi mdi-select-multiple")),

          div(className := "iconBtn separator"),
          div(className := "iconBtn separator"),
          div(
            className := "iconBtn",
            onClick := (_ => navMgr.reload())
          )(span(title := "RELOAD", className := "mdi mdi-refresh")),
        ),
        hr(),
        div(className := "breadcrumbHolder")(
          navMgr.parents.zipWithIndex.reverse.map { case (FullNavNode(navNode, viewNode), idx) =>
            div(
              key := navNode.nodes.map(_.path).mkString(","),
              className := "breadcrumb textBtn",
              onClick := (_ => navMgr.up(idx))
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
        table(
          className := (if (fetchMgr.loading) "nodeTable loading" else "nodeTable")
        )(
          thead(
            tr(
              onDragLeave := (_ => setDragColOver(None))
            )(
              colMgr.columns.zipWithIndex.map { case (c, idx) =>
                td(
                  key := c.name,
                  className := {
                    val nowrap = if (allowWrap(c.name)) "" else " noWrap"
                    if (dragColOver.contains(c.name) && idx < dragIdx) "dragover-left" + nowrap
                    else if (dragColOver.contains(c.name) && idx > dragIdx) "dragover-right" + nowrap
                    else if (dragCol.contains(c.name)) "dragging" + nowrap
                    else "" + nowrap
                  },
                  draggable := "true",
                  onDragOver := { e => e.preventDefault(); setDragColOver(Some(c.name)) },
                  onDragStart := (_ => setDragCol(Some(c.name))),
                  onDragEnd := { _ => setDragCol(None); setDragColOver(None) },
                  onDrop := { e => e.preventDefault(); dragCol.foreach(colMgr.move(_, idx)) },
                  onContextMenu := { e =>
                    e.preventDefault()
                    headerMenu.current.style = s"top: ${e.pageY}px; left:${e.pageX}px;"
                    setHeaderMenuActive(true)
                  }
                )(c.name)
              })
          ),
          tbody(
            tr(
              hidden := (navMgr.parents.size <= 1 || fetchMgr.loading),
              className := "nodeRow",
              onDoubleClick := (_ => navMgr.up())
            )(
              colMgr.columns.map(_.name).map {
                case c@"typ" => td(key := c)("D")
                case c@"hash" => td(key := c)("")
                case c@"name" => td(key := c)("..")
                case c => td(key := c)("0")
              }
            ),
            aggrNodes.take(fetchMgr.limit).zipWithIndex.map { case (node, idx) =>
              tr(
                key := node.path,
                className := Seq(
                  "nodeRow",
                  if (selMgr.selected.contains(node)) "selectedRow" else "",
                  node.mark match {
                    case Some(MarkType.Ignored) => "markedIgnored"
                    case Some(MarkType.Deleted) => "markedDeleted"
                    case Some(MarkType.Saved) => "markedSaved"
                    case None => ""
                  }
                ).mkString(" "),
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
                colMgr.columns.map(c =>
                  td(
                    key := c.name,
                    className := (if (allowWrap(c.name)) "" else "noWrap")
                  )(c.get(node))
                )
              )
            }
          )
        ),
        div(hidden := !fetchMgr.loading)("Loading..."),
        div(hidden := fetchMgr.children.size <= fetchMgr.limit)(
          div(className := "textNoBtn")(s"Omitted ${fetchMgr.children.size - fetchMgr.limit} rows."),
          div(
            className := "textBtn", onClick := (_ => fetchMgr.incLimitBy(1000))
          )("[+1000]"),
          div(
            className := "textBtn", onClick := (_ => fetchMgr.noLimit())
          )(s"[+${fetchMgr.children.size - fetchMgr.limit}]"),
        ),

        // context menu
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
                  .setFullPath(true))
              fetchMgr.sortByPath()
            }
          )(s"DOUBLES (${selMgr.selected.map(_.hash).size})"),
          hr(className := "divider"),
          div(
            className := "item selectable",
            onClick := { _ =>
              mark(MarkReq(MarkType.Ignored, selMgr.selected.toList))
            }
          )(s"MARK IGNORED (${selMgr.selected.size})"),
          div(
            className := "item selectable",
            onClick := { _ =>
              mark(MarkReq(MarkType.Deleted, selMgr.selected.toList))
            }
          )(s"MARK DELETED (${selMgr.selected.size})"),
          div(
            className := "item selectable",
            onClick := { _ =>
              mark(MarkReq(MarkType.Saved, selMgr.selected.toList))
            }
          )(s"MARK SAVED (${selMgr.selected.size})"),
          div(
            className := "item selectable",
            onClick := { _ =>
              mark(MarkReq(None, selMgr.selected.toList))
            }
          )(s"UNMARK (${selMgr.selected.size})"),
        ),


        // header choice menu
        div(
          hidden := !headerMenuActive,
          className := "ctxMenu",
          ref := headerMenu
        )(
          colMgr.colHeaders.map(h =>
            div(
              key := h.name,
              className := "item selectable condensed",
              onClick := { e => e.stopPropagation(); colMgr.toggle(h.name) }
            )(s"[${if (h.visible) "o" else " "}] ${h.name}"),
          )

        )

      )
    )


  }

}

case class NodeCol[B](name: String)(transform: NodeLite => Option[B]) extends ColMgr.ColDef[NodeLite] {
  def get(node: NodeLite): Option[String] = transform(node).map(_.toString)
}
object NodeCol {
  def of[B](name: String)(transform: NodeLite => B): NodeCol[B] =
    NodeCol(name)(transform andThen (x => Some(x)))
}

case class NodeStatCol[A <: Stat, B](name: String, key: StatKey[A])(transform: A => B) extends ColMgr.ColDef[NodeLite] {
  def get(node: NodeLite): Option[String] =
    get(node.stats)
  def get(stats: Stats): Option[String] =
    stats(key).map(transform).map(_.toString)
}