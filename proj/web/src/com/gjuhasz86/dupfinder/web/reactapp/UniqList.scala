package com.gjuhasz86.dupfinder.web.reactapp

class UniqList[+A] private(val inner: List[A] = Nil) extends AnyVal {
  import UniqList.UniqOnList

  def +[B >: A](a: B): UniqList[B] = (a :: inner).uniq
  def -[B >: A](a: B): UniqList[B] = inner.filterNot(_ == a).uniq
  def ++[B >: A](other: Seq[B]): UniqList[B] = (inner ++ other).uniq
  def ++[B >: A](other: UniqList[B]): UniqList[B] = (inner ++ other.inner).uniq
  def --[B >: A](other: Seq[B]): UniqList[B] = inner.filterNot(other.contains).uniq
  def --[B >: A](other: UniqList[B]): UniqList[B] = inner.filterNot(other.contains).uniq
  def contains[B >: A](elem: B): Boolean = inner.contains(elem)
  def map[B](f: A => B): UniqList[B] = inner.map(f).uniq
  def foldLeft[B](acc: B)(f: (B, A) => B): B = inner.foldLeft(acc)(f)
  def size: Int = inner.size
  def isEmpty: Boolean = inner.isEmpty
  def toList: List[A] = inner
  def toSet[B >: A]: Set[B] = inner.toSet
}

object UniqList {
  def apply[A](inner: List[A] = Nil): UniqList[A] = new UniqList(inner.distinct)
  //    implicit def fromList[A](list: List[A]): UniqList[A] = new UniqList(list)
  implicit class UniqOnList[A](val self: List[A]) extends AnyVal {
    def uniq: UniqList[A] = UniqList(self)
  }
}