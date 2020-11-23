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

    val (panelCount, setPanelCount) = useState(1)

    div(
      (1 to panelCount).toList.map { i =>
        Panel()
      },
      div(
        className := "textBtn addPanel",
        onClick := (_ => setPanelCount(panelCount + 1))
      )("[ADD PANEL]")
    )
  }
}
