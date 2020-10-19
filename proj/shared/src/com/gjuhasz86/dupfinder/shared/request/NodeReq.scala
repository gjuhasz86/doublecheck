package com.gjuhasz86.dupfinder.shared.request

case class NodeReq(roots: Set[String], selection: ChildSelection, filters: Set[ChildFilter])

sealed trait ChildSelection
object ChildSelection {
  case object Direct extends ChildSelection
  case object All extends ChildSelection
}

sealed trait ChildFilter
object ChildFilter {
  case class NodeTypeIn(ntypes: Set[String]) extends ChildFilter
  case object NonEmpty extends ChildFilter
  case object HasDups extends ChildFilter
  case object HasExtDups extends ChildFilter
}