package scalacats.chapter4

import cats.Monad

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)
}

object TreeInstances {
  implicit def treeMonad[A]: Monad[Tree] = new TreeMonad[A]
}

class TreeMonad[A] extends Monad[Tree] {
  def pure[A](x: A): Tree[A] = Leaf(x)

  def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
    fa match {
      case Leaf(a) => f(a)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

  def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
    flatMap(f(a)) {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => Leaf(value)
    }
  }
}
