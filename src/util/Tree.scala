package util

abstract class Tree[T](value: T)
case class Node[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T](value)
case class Leaf[T](value: T) extends Tree[T](value)


