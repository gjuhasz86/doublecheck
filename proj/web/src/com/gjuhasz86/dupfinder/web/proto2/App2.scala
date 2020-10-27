package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._
import scala.collection.decorators._

@react class App2 extends StatelessComponent {

  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  def fetchRoot(): Unit =
    FetchUtils.getBackend("root") { res =>
      val Right(root) = decode[Node](res)
      navMgr.current.root(root)
    }

  type Props = Unit
  val navMgr: ReactRef[NavManager] = React.createRef[NavManager.Def]
  val chldMgr: ReactRef[ChildrenManager] = React.createRef[ChildrenManager.Def]
  val selMgr: ReactRef[SelectionManager] = React.createRef[SelectionManager.Def]


  override def componentDidMount(): Unit = {
    fetchRoot()
  }

  def render(): ReactElement = {
    div(
      NavManager(onCurrentNodeChange = { root =>
        chldMgr.current.setRoot(root)
        selMgr.current.clear()
      })(navMgrState =>
        ChildrenManager(chldMgrState =>
          SelectionManager(chldMgrState.children)(selMgrState =>
            Fragment(
              div(className := "breadcrumbHolder")(
                navMgrState.parents.zipWithIndex.reverse.map { case (node, idx) =>
                  div(
                    className := "breadcrumb textBtn",
                    onClick := (_ => navMgr.current.up(idx))
                  )(node.name)
                }.intersperse(
                  div(className := "breadcrumb")(">")
                )
              ),
              div(selMgrState.selected.size),
              table(
                thead(
                  tr(td("typ"), td("name"), td("cld"), td("dmy"), td("nzf"), td("dup"), td("ext"), td("sdc"))
                ),
                tbody(
                  tr(
                    className := "nodeRow",
                    onClick := (_ => navMgr.current.up())
                  )(
                    td("D"), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
                  ),
                  chldMgrState.children.zipWithIndex.map { case (node, idx) =>
                    tr(
                      className := (if (selMgrState.selected.contains(node)) "nodeRow selectedRow" else "nodeRow"),
                      onDoubleClick := (_ => navMgr.current.down(node)),
                      onClick := {
                        case e if e.ctrlKey => selMgr.current.toggle(node)
                        case e if e.shiftKey => selMgr.current.addRange(idx)
                        case _ => selMgr.current.cleanAdd(node)
                      },
                      onMouseDown := { e => selMgr.current.dragFrom(node) },
                      onMouseEnter := { case e if e.buttons == 1 => selMgr.current.dragOn(node) case _ => },
                      onMouseLeave := { case e if e.buttons == 1 => selMgr.current.dragOn(node) case _ => },
                    )(
                      td(node.ntype),
                      td(node.name),
                      td(node.childCount),
                      td(node.dummyCount),
                      td(node.stats.getOrElse[Int]("F", 0)),
                      td(node.dupCount),
                      td(node.extDupCount),
                      td(node.selfDupCount)
                    )
                  }
                )
              )
            )
          ).withRef(selMgr)
        ).withRef(chldMgr)
      ).withRef(navMgr)
    )
  }
}