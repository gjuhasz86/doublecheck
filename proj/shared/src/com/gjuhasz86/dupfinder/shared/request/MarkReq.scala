package com.gjuhasz86.dupfinder.shared.request
import com.gjuhasz86.dupfinder.shared.NodeLite

case class MarkReq(markType: Option[MarkType], nodes: List[NodeLite])
object MarkReq {
  def apply(markType: MarkType, nodes: List[NodeLite]): MarkReq =
    new MarkReq(Some(markType), nodes)
}

sealed trait MarkType
object MarkType {
  case object Ignored extends MarkType
  case object Deleted extends MarkType
  case object Saved extends MarkType
}
