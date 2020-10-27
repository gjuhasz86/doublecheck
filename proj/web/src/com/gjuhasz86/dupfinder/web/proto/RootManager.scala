package com.gjuhasz86.dupfinder.web.proto

import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Node
import com.raquo.laminar.api.L._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import org.scalajs.dom.html

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RootManager extends Component[html.Div] {
  implicit private val customConfig: Configuration = Configuration.default.withDefaults

  override val rel = div(hidden := true)
  val root = EventStream.fromFuture(fetchRoot)

  private def fetchRoot: Future[Node] =
    FetchUtils.getPath("root").map { res =>
      decode[Node](res).getOrElse(null)
    }
}