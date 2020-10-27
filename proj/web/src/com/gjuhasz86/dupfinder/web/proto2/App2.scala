package com.gjuhasz86.dupfinder.web.proto2

import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildSelection
import com.gjuhasz86.dupfinder.web.Node
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.Fragment
import slinky.core.facade.React
import slinky.core.facade.ReactElement
import slinky.core.facade.ReactRef
import slinky.web.html._

@react class App2 extends StatelessComponent {

  type Props = Unit
  val navMgr: ReactRef[NavManager] = React.createRef[NavManager.Def]

  def render(): ReactElement = {
    div(
      NavManager(navMgrState =>
        Fragment(
          div("hello"),
          div(navMgrState.current.name),
          div(),
          div(onClick := (_ => navMgr.current.root(Node.Empty.copy(name = "foo"))), "clickme 1"),
          div(onClick := (_ => println("clicked me")), "clickme 2")
        )
      ).withRef(navMgr)
    )
  }
}