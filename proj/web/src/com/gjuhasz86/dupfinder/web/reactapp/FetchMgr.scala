package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import com.gjuhasz86.dupfinder.web.Utils.skDec
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core.facade.Hooks._


case class ChildrenMgrState0(loading: Boolean, children: List[NodeLite], sorting: Ordering[NodeLite], limit: Int)

trait FetchMgr {
  def loading: Boolean
  def children: List[NodeLite]
  def limit: Int
  def ord: Ordering[NodeLite]

  def setLimit(n: Int): Unit
  def incLimitBy(n: Int): Unit
  def noLimit(): Unit
  def sortByName(): Unit
  def sortByPath(): Unit
  def loadChildren(req: NodeReq): Unit
}

object FetchMgr {
  val ordByName: Ordering[NodeLite] = Ordering.by(n => (n.ntype, n.name))
  val ordByPath: Ordering[NodeLite] = Ordering.by(_.path)

  def useChildren: FetchMgr = {

    val (loadingState, setLoading) = useState(false)
    val (limitState, setLimitState) = useState(1000)
    val (ordering, setOrdering) = useState(ordByName)
    val (childrenState, setChildren) = useState(List[NodeLite]())

    def fetchNodes(req: NodeReq): Unit = {
      setLoading(true)
      FetchUtils.postBackend("searchLite", req.asJson.noSpaces) { res =>
        val Right(nodes) = decode[List[NodeLite]](res)
        setLoading(false)
        setChildren(nodes.sorted(ordering))
        setLimitState(1000)
      }
    }
    new FetchMgr {

      def loading = loadingState
      def children = childrenState
      def limit = limitState
      def ord = ordering

      def setLimit(n: Int) = setLimitState(n)
      def incLimitBy(n: Int) = setLimitState(limit + n)
      def noLimit() = setLimit(children.size)
      def sortByName() = {
        setOrdering(ordByName)
        setChildren(children.sorted(ordByName))
      }

      def sortByPath() = {
        setOrdering(ordByPath)
        setChildren(children.sorted(ordByPath))
      }

      def loadChildren(req: NodeReq) =
        fetchNodes(req)
    }

  }

}
