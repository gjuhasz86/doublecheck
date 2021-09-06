package com.gjuhasz86.dupfinder.backend.core

import better.files._

import scala.annotation.tailrec
import com.gjuhasz86.dupfinder.backend.core.Utils._
import com.gjuhasz86.dupfinder.shared.NodeType
import com.gjuhasz86.dupfinder.shared.NodeType._

// Generates a list of files with metadataas nodes.csv
// from a given directory (passed in as a command line argument)
object Preparation {

  def main(args: Array[String]): Unit = {
    val (root,dirs) = args.toList.map(_.toFile) match {
      case dir :: Nil => 
        (dir.parent, dir :: Nil)
      case root :: dirs =>
        (root, dirs)
      case _ =>
        throw new IllegalArgumentException("Expected one or more arguments")
    }
    
    val outputFile = "dist/data/nodes.csv".toFile
    outputFile.clear()
    outputFile.parent.createDirectories()

    val nodes = walkDir(dirs, Nil)
    nodes.reverse.foreach { node =>
      val path = node.file.relativeTo(root)
      val row = s"${node.ntype.short},${node.size},$path"
      outputFile.appendLine(row)
    }

  }

  var counter = 0

  @tailrec
  def walkDir(remaining: List[File], acc: List[FileNode]): List[FileNode] = {
    counter += 1
    if (counter % 50000 == 0) {println(counter)}
    remaining match {
      case Nil =>
        acc
      case curr :: rest if curr.isDirectory && curr.isReadable && !curr.isSymbolicLink =>
        walkDir(curr.list.toList.reverse ::: rest, FileNode.of(curr) :: acc)
      case curr :: rest =>
        walkDir(rest, FileNode.of(curr) :: acc)
    }
  }

  case class FileNode(file: File, ntype: NodeType, size: Long)

  object FileNode {
    def of(f: File) =
      f match {
        case s if s.isSymbolicLink =>
          FileNode(s, Lnk, 0)
        case d if d.isDirectory && !d.isReadable =>
          FileNode(d, Dir(false), 0)
        case d if d.isDirectory && d.isReadable =>
          FileNode(d, Dir(true), 0)
        case f if f.isRegularFile =>
          FileNode(f, Fil(f.isReadable), f.size)
        case f =>
          FileNode(f, Oth, 0)
      }
  }
}
