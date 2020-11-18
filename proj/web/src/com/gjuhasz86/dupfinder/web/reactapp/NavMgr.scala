package com.gjuhasz86.dupfinder.web.reactapp

import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.request.ChildFilter
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.Empty
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.HasExtDups
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NodeTypeIn
import com.gjuhasz86.dupfinder.shared.request.ChildFilter.NonEmpty
import com.gjuhasz86.dupfinder.shared.request.NodeSelection
import slinky.core.facade.Hooks._

trait NavMgr {
  def parents: List[FullNavNode]
  def nextNavNode: FullNavNode
  def changeNextNavNode(f: FullNavNode => FullNavNode): Unit

  def root(node: NodeLite): Unit
  def root(node: NodeLite, navNode: FullNavNode): Unit
  def up(): Unit
  def up(n: Int): Unit
  def down(node: NodeLite): Unit
  def down(nodes: List[NodeLite]): Unit
  def down(node: NodeLite, navNode: FullNavNode): Unit
  def down(nodes: List[NodeLite], navNode: FullNavNode): Unit
}

case class ViewNode(fullPath: Boolean, aggregate: Boolean, manual: Boolean)
case class FullNavNode(navNode: NavNode, viewNode: ViewNode) {
  def setFullPath(enabled: Boolean) =
    copy(viewNode = viewNode.copy(fullPath = enabled))

  def setAggregate(enabled: Boolean) =
    copy(viewNode = viewNode.copy(aggregate = enabled))

  def setSelection(sel: NodeSelection) =
    copy(navNode = navNode.copy(selection = sel))

  def toggleFilter(cf: ChildFilter) =
    if (navNode.filter.contains(cf)) remFilter(cf) else addFilter(cf)

  def addFilter(cf: ChildFilter) =
    copy(navNode = navNode.copy(filter = navNode.filter -- NavMgr.conflicts(cf) + cf))

  def remFilter(cf: ChildFilter) =
    copy(navNode = navNode.copy(filter = navNode.filter - cf))

  def setNode(node: NodeLite) =
    setNodes(List(node))

  def setNodes(nodes: List[NodeLite]) =
    copy(navNode = navNode.copy(nodes = nodes))

  def setManual(m: Boolean) =
    copy(viewNode = viewNode.copy(manual = m))

}


object NavMgr {
  val conflicts =
    Map[ChildFilter, Set[ChildFilter]](
      Empty -> Set(NonEmpty),
      NonEmpty -> Set(Empty),
      HasExtDups -> Set(HasDups),
      HasDups -> Set(HasExtDups),
      NodeTypeIn(Set("D")) -> Set(NodeTypeIn(Set("F"))),
      NodeTypeIn(Set("F")) -> Set(NodeTypeIn(Set("D")))
    ).withDefaultValue(Set())

  def useNavigation(onCurrentChange: FullNavNode => Unit) = {
    val (parentsState, setParents) = useState(List[FullNavNode]())
    val (nextNavNodeState, setNextNavNode) = useState(FullNavNode(NavNode.default, ViewNode(false, false, true)).addFilter(NonEmpty))

    def setParentsWithCallback(nodes: List[FullNavNode]) = {
      val invoke = parentsState.headOption.forall(_.navNode != nodes.head.navNode)
      setParents(nodes)
      if (invoke) {onCurrentChange(nodes.head)}
    }

    new NavMgr {
      def parents = parentsState
      def current = parentsState.headOption.getOrElse(FullNavNode(NavNode.default, ViewNode(false, false, true)))

      def nextNavNode = nextNavNodeState

      def changeNextNavNode(f: FullNavNode => FullNavNode) =
        setNextNavNode(f(nextNavNodeState))

      def changeCurrNavNode(f: FullNavNode => FullNavNode) =
        parents match {
          case head :: tail =>
            setParentsWithCallback(f(head) :: tail)
          case Nil =>
        }

      def syncNext() =
        parentsState match {
          case head :: _ => setNextNavNode(head)
          case Nil =>
        }

      def root(node: NodeLite) =
        root(node, nextNavNodeState)

      def root(node: NodeLite, navNode: FullNavNode) =
        setParentsWithCallback(List(navNode.setNode(node)))

      def up() = up(1)

      def up(n: Int) =
        setParentsWithCallback(parentsState.drop(n min (parentsState.size - 1)))

      def down(node: NodeLite) =
        down(List(node))

      def down(nodes: List[NodeLite]) =
        down(nodes, nextNavNodeState)

      def down(node: NodeLite, navNode: FullNavNode) =
        down(List(node), navNode)

      def down(nodes: List[NodeLite], navNode: FullNavNode) =
        setParentsWithCallback(navNode.setNodes(nodes) :: parentsState)

    }

  }

}
