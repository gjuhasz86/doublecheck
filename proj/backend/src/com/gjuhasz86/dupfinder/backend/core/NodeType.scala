package com.gjuhasz86.dupfinder.backend.core

sealed trait NodeType

object NodeType {
  case class Dir(readable: Boolean) extends NodeType
  case class Fil(readable: Boolean) extends NodeType
  case object Lnk extends NodeType
  case object Oth extends NodeType

  implicit class ShortOnNodeType(val self: NodeType) extends AnyVal {
    def short = self match {
      case Dir(true) => "D"
      case Dir(false) => "d"
      case Fil(true) => "F"
      case Fil(false) => "f"
      case Lnk => "L"
      case Oth => "O"
    }
  }

  val AllInstances = List(Dir(true), Dir(false), Fil(true), Fil(false), Lnk, Oth)

  val fromShort: Map[String, NodeType] = AllInstances.map(x => x.short -> x).toMap

  def unapply(str: String): Option[NodeType] = fromShort.get(str)

}