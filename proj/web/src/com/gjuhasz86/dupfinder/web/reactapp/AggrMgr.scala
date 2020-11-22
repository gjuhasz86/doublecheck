package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import scala.annotation.tailrec

object AggrMgr {

  @tailrec
  private def aggregate(list: List[NodeLite], acc: List[NodeLite]): List[NodeLite] =
    list match {
      case a :: rest if a.ntype != "D" => aggregate(rest, acc)
      case a :: b :: rest if a.hashes == b.hashes => aggregate(b :: rest, acc)
      case a :: rest => aggregate(rest, a :: acc)
      case Nil => acc
    }

  def useAggregation(nodesProp: List[NodeLite], enabled: Boolean) =
    if (enabled)
      aggregate(nodesProp.sortBy(_.path), Nil).reverse
    else
      nodesProp
}
