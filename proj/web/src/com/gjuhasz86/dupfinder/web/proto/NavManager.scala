package com.gjuhasz86.dupfinder.web.proto

import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveElement
import org.scalajs.dom.html

object NavManager {
  sealed trait NavCmd
  object NavCmd {
    case class Up(n: Int = 1) extends NavCmd
    case class Down(node: FlatNode) extends NavCmd
    case class Root(node: FlatNode) extends NavCmd
  }
}
class NavManager extends Component[html.Div] {
  import NavManager._

  val navBus: EventBus[NavCmd] = new EventBus[NavCmd]
  val rootBus: EventBus[FlatNode] = new EventBus[FlatNode]

  val parentsStack: Signal[List[FlatNode]] = {
    navBus.events.foldLeft(List[FlatNode]())((parents, nav) => nav match {
      case NavCmd.Root(node) => List(node)
      case NavCmd.Up(n) => parents.drop(n min (parents.size - 1))
      case NavCmd.Down(node) => node :: parents
    })
  }

  val currentNode: Signal[FlatNode] = parentsStack.map(_.headOption.getOrElse(FlatNode.Empty))

  override val rel: ReactiveElement[html.Div] =
    div(
      hidden := true,
      rootBus.events.map(NavCmd.Root) --> navBus.writer
    )
}