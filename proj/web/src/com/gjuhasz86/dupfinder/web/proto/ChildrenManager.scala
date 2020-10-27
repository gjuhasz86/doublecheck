package com.gjuhasz86.dupfinder.web.proto

import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.signal.Signal
import com.raquo.laminar.api.L._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom.html

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ChildrenManager extends Component[html.Div] {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  override val rel = div(hidden := true)

  val root: EventBus[Node] = new EventBus[Node]

  val children: Signal[List[Node]] =
    root.events
      .flatMap(node => EventStream.fromFuture(fetchChildren(node)))
      .map(_.sortBy(n => (n.ntype, n.name)))
      .toSignal(Nil)

  private def fetchChildren(node: Node): Future[List[Node]] =
    FetchUtils.postPath("children", node.path.asJson.noSpaces).map { res =>
      decode[List[Node]](res).getOrElse(null)
    }
}