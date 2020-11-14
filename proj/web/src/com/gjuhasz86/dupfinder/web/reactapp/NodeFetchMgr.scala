package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.gjuhasz86.dupfinder.web.FetchUtils
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import slinky.core.facade.Hooks._

case class ChildrenMgrState0(loading: Boolean, children: List[NodeLite], sorting: Ordering[NodeLite], limit: Int)

object NodeFetchMgr {
  def initialState = ChildrenMgrState(loading = false, Nil, Ordering.by(n => (n.ntype, n.name)), 1000)

  def useChildren: FetchMgr = {

    val (loadingState, setLoading) = useState(false)
    val (limitState, setLimitState) = useState(1000)
    val (ordering, setOrdering) = useState(Ordering.by((n: NodeLite) => (n.ntype, n.name)))
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
      def setLimit(n: Int) = setLimitState(n)
      def incLimitBy(n: Int) = setLimitState(limit + n)
      def noLimit() = setLimit(children.size)
      def sortByName() = {
        val ord: Ordering[NodeLite] = Ordering.by(n => (n.ntype, n.name))
        setOrdering(ord)
        setChildren(children.sorted(ord))
      }

      def sortByPath() = {
        val ord: Ordering[NodeLite] = Ordering.by(_.path)
        setOrdering(ord)
        setChildren(children.sorted(ord))
      }

      def loadChildren(req: NodeReq) =
        fetchNodes(req)
    }

  }

  trait FetchMgr {
    def loading: Boolean
    def children: List[NodeLite]
    def limit: Int

    def setLimit(n: Int): Unit
    def incLimitBy(n: Int): Unit
    def noLimit(): Unit
    def sortByName(): Unit
    def sortByPath(): Unit
    def loadChildren(req: NodeReq): Unit
  }

}
