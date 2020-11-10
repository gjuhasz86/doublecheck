package com.gjuhasz86.dupfinder.web.laminarapp

import com.raquo.airstream.eventbus.EventBus
import com.raquo.laminar.api.L._
import io.circe.generic.extras.Configuration
import org.scalajs.dom

import scala.collection.decorators._
import scala.scalajs.js.annotation.JSExportTopLevel

object Main2 {
  import ImplicitHelpers._
  import NavManager._
  import SelectionManager._

  implicit private val customConfig: Configuration = Configuration.default.withDefaults


  //  @JSExportTopLevel("main")
  def main(): Unit = {

    val navMgr = new NavManager
    val chdMgr = new ChildrenManager
    val rootMgr = new RootManager
    val selMgr = new SelectionManager[FlatNode]

    val debugBus = new EventBus[Any]
    debugBus.events.foreach(x => println(s"DEBUG: $x"))(new Owner {})

    val rootElement = div(
      navMgr,
      chdMgr,
      rootMgr,
      selMgr,
      navMgr.currentNode --> chdMgr.root.writer,
      rootMgr.root --> navMgr.rootBus,
      chdMgr.children --> selMgr.items.writer,
      chdMgr.root.events.mapTo(SelCmd.SelEvent(SelCmd.Op.CleanAdd, Set(), 0)) --> selMgr.command,
      div(
        cls := "breadcrumbHolder",
        children <-- navMgr.parentsStack.map(_.zipWithIndex.reverse).splitIntoSignals(_._1.path) { (_, _, nodeIdx) =>
          val node = nodeIdx.map(_._1)
          val idx = nodeIdx.map(_._2)
          div(
            cls := "breadcrumb", cls := "textBtn",
            inContext(_.events(onClick).sample(idx).map(NavCmd.Up.apply) --> navMgr.navBus.writer),
            node.asText(_.name))
        }.map(x => x.intersperse(div(cls := "breadcrumb", ">")))
      ),
      div(selMgr.selectedItems.map(_.size).signal.asText(_.toString)),
      table(
        //        inContext(_.events(onMouseEnter).map(_.buttons == 1) --> selMgr.dragSelectVar.writer),
        //        inContext(_.events(onMouseLeave).map(_.buttons == 1) --> selMgr.dragSelectVar.writer),
        thead(
          tr(td("typ"), td("name"), td("cld"), td("dmy"), td("nzf"), td("dup"), td("ext"), td("sdc"))
        ),
        tbody(
          tr(
            hidden <-- navMgr.parentsStack.map(_.size <= 1),
            onDblClick.mapTo(NavCmd.Up()) --> navMgr.navBus.writer,
            cls := "nodeRow",
            td("D"), td(".."), td("0"), td("0"), td("0"), td("0"), td("0"), td("0")
          ),
          children <-- chdMgr.children.map(_.zipWithIndex.map { case (node, idx) =>
            tr(
              cls := "nodeRow",
              cls.toggle("selectedRow") <-- selMgr.selectedItems.map(_.contains(node)),
              inContext(_.events(onDblClick).mapToValue(node).map(NavCmd.Down.apply) --> navMgr.navBus.writer),
              inContext(_.events(onClick).collect {
                case e if e.ctrlKey => SelCmd.ByItem(SelCmd.Op.Toggle, node)
                case e if e.shiftKey => SelCmd.ByRangeCont(SelCmd.Op.Add, idx)
                case _ => SelCmd.ByItem(SelCmd.Op.CleanAdd, node)
              } --> selMgr.command),
              //              inContext(_.events(onContextMenu, preventDefault = true).mapToValue(s"CTX [$node]") --> debugBus.writer),
              inContext(_.events(onMouseDown).sample(selMgr.selectedItems).map(x => if (x.contains(node)) SelCmd.Op.Rem else SelCmd.Op.Add) --> selMgr.dragSelectVar.writer),
              inContext(_.events(onMouseEnter).filter(_.buttons == 1).sample(selMgr.dragSelectVar.signal).map(op => SelCmd.ByItem(op, node)) --> selMgr.command),
              inContext(_.events(onMouseLeave).filter(_.buttons == 1).sample(selMgr.dragSelectVar.signal).map(op => SelCmd.ByItem(op, node)) --> selMgr.command),
              td(node.ntype),
              td(node.name),
              td(node.childCount),
              td(node.dummyCount),
              td(node.stats.getOrElse[Int]("F", 0)),
              td(node.dupCount),
              td(node.extDupCount),
              td(node.selfDupCount),
            )
          })
        )
      )
    )

    val containerNode = dom.document.querySelector("#root")
    render(containerNode, rootElement)
  }


}
