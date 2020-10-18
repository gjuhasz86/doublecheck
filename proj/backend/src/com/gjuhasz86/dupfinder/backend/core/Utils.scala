package com.gjuhasz86.dupfinder.backend.core

import better.files._
import java.nio.file.Path

import scala.annotation.tailrec

object Utils {
  def mergeMap[K](m1: Map[K, Int], m2: Map[K, Int]): Map[K, Int] = {
    (m1.keySet ++ m2.keySet)
      .map { k => k -> (m1.getOrElse(k, 0) + m2.getOrElse(k, 0)) }
      .toMap
  }

  implicit class MergeOnMaps[K](val self: List[Map[K, Int]]) extends AnyVal {
    def merged: Map[K, Int] = self.foldLeft(Map[K, Int]())(mergeMap)
  }

  implicit class RelOnFile(val self: File) extends AnyVal {
    def relativeTo(root: File): Path = root.relativize(self)
  }

  implicit class AllParentsOnPath(val self: Path) extends AnyVal {
    def allParents: List[File] = {
      @tailrec
      def loop(acc: List[File], path: Path): List[File] = {
        path.getParent match {
          case null => acc
          case p => loop(p :: acc, p)
        }
      }
      loop(Nil, self)
    }
  }
  val DummyHash = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
}
