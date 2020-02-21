package scalacats.chapter4

import cats.{Id, Monad}

class IdMonad extends Monad[Id] {
  def pure[A](x: A): Id[A] = x
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): B = f(fa)
  def tailRecM[A, B](a: Id[A])(f: A => Id[Either[A, B]]): Id[B] = f(a).right.get
}

