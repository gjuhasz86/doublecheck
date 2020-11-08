package com.gjuhasz86.dupfinder.shared.request

case class NodeReq(roots: Set[String], selection: NodeSelection, filters: Set[ChildFilter])

sealed trait NodeSelection
object NodeSelection {
  case object DirectChildren extends NodeSelection
  case object DeepChildren extends NodeSelection
  case object DupNodes extends NodeSelection
}

sealed trait ChildFilter
object ChildFilter {
  case class NodeTypeIn(ntypes: Set[String]) extends ChildFilter
  case object NonEmpty extends ChildFilter
  case object Empty extends ChildFilter
  case object HasDups extends ChildFilter
  case object HasExtDups extends ChildFilter
}